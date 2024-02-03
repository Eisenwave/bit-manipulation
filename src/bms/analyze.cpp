#include <unordered_map>

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"
#include "bms/parse_number.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

Analyzer_Base::Analyzer_Base(Parsed_Program& program)
    : m_program(program)
    , m_root(std::get<ast::Program_Node>(program.get_node(program.root_node)))
{
}

ast::Some_Node& Analyzer_Base::get_node(ast::Node_Handle handle)
{
    return m_program.get_node(handle);
}

namespace {

/// @brief Class responsible for performing instantiations.
/// After runnings this analyzer, functions will have attached instance nodes.
/// These instance nodes are clones of the original function node but where the bit-generic
/// parameter types are replaced with concrete types.
/// Also, the id-expression within the bit-generic type will have a constant value, which
/// facilitates constant propagation in further stages.
struct Instantiator : Analyzer_Base {

    Instantiator(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Result<void, Analysis_Error> operator()(int width)
    {
        return instantiate(width);
    }

private:
    Result<void, Analysis_Error> instantiate(int width)
    {
        BIT_MANIPULATION_ASSERT(width > 0);
        BIT_MANIPULATION_ASSERT(width <= uint_max_width);

        for (Node_Handle decl : m_root.declarations) {
            if (auto* f = std::get_if<Function_Node>(&get_node(decl))) {
                Node_Handle instance = clone_node(decl);
                auto& instance_node = std::get<Function_Node>(get_node(instance));
                const auto r = instantiate(instance_node, width);
                if (!r) {
                    return r;
                }
                f->instances.push_back(instance);
            }
        }

        return {};
    }

    Result<void, Analysis_Error> instantiate([[maybe_unused]] Function_Node& instance,
                                             [[maybe_unused]] int width)
    {
        auto& params = std::get<Parameter_List_Node>(get_node(instance.get_parameters()));
        for (Node_Handle p : params.parameters) {
            auto& param_node = std::get<Parameter_Node>(get_node(p));
            auto& type_node = std::get<Type_Node>(get_node(param_node.get_type()));
            if (auto* generic_type = std::get_if<Bit_Generic_Type>(&type_node.type)) {
                BIT_MANIPULATION_ASSERT(generic_type->type == Type_Type::Uint);
                auto* id_node = std::get_if<Id_Expression_Node>(&get_node(generic_type->width));
                // If it isn't an id-expression, it cannot be a generic parameter.
                if (id_node && id_node->bit_generic) {
                    // Merely replacing the type isn't enough; we must also give this node a
                    // constant value so that existing name lookup can obtain this value.
                    id_node->const_value = Value { Concrete_Type::Int, width };
                    type_node.type = Concrete_Type::Uint(width);
                }
            }
        }

        return {};
    }

    Node_Handle clone_node(Node_Handle h)
    {
        // Two passes are necessary.
        // During the first pass, the nodes are all copied and the links to children are updated
        // in the copies.
        // However, this does not yet affect the name lookup results, which must also refer to
        // the cloned nodes.
        // This is done in a second pass, since cloning a cyclic graph in one go is difficult.
        std::unordered_map<Node_Handle, Node_Handle> remap;
        const Node_Handle result = clone_node_first_pass(h, remap);
        clone_node_second_pass(h, remap);

        return result;
    }

    Node_Handle clone_node_first_pass(Node_Handle h,
                                      std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        const Node_Handle result
            = std::visit([this]<typename T>(const T& n) { return m_program.push_node(T(n)); },
                         m_program.get_node(h));
        remap.emplace(h, result);

        for (Node_Handle& child : get_children(m_program.get_node(result))) {
            child = clone_node_first_pass(child, remap);
        }
        return result;
    }

    void clone_node_second_pass(Node_Handle h,
                                const std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        std::visit(
            [this, &h, &remap]<typename T>(T& n) {
                if constexpr (Lookup_Performing_Node<T>) {
                    n.lookup_result = remap.at(h);
                }
                for (Node_Handle child : get_children(n)) {
                    clone_node_second_pass(child, remap);
                }
            },
            m_program.get_node(h));
    }
};

struct Type_Analyzer : Analyzer_Base {
private:
    enum struct Context {
        // The default context. Full analysis of functions and everything inside.
        regular,
        // Return type context. Information is stored in `return_info`.
        return_type,
        // A constant expression. Constant evaluation cannot run into an unknown value.
        constant_expression,
        // A function call. Limited analysis is performed, only to determine if a call is valid.
        function_call
    };

    struct Return_Info {
        Concrete_Type type;
        Token token;
    };

    std::optional<Return_Info> return_info;

public:
    Type_Analyzer(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        return analyze_types(m_program.root_node, Context::regular);
    }

private:
    Result<void, Analysis_Error> analyze_types(Node_Handle handle, Context context)
    {
        if (handle == Node_Handle::null) {
            return {};
        }
        return std::visit([this, context](auto& node) { return analyze_types(node, context); },
                          get_node(handle));
    }

    template <typename T>
        requires(!std::is_const_v<T>)
    Result<void, Analysis_Error> analyze_types(T& node, Context context)
    {
        for (Node_Handle h : node.get_children()) {
            auto r = analyze_types(h, context);
            if (!r) {
                return r;
            }
        }
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Function_Node& node, Context context)
    {
        if (node.is_generic) {
            for (Node_Handle instance : node.instances) {
                auto r = analyze_types(instance, context);
                if (!r) {
                    return r;
                }
            }
            return {};
        }
        if (auto r = analyze_types(node.get_parameters(), context); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_return_type(), Context::return_type); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_requires_clause(), Context::constant_expression); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_body(), context); !r) {
            return r;
        }
        node.const_value = get_const_value(get_node(node.get_return_type()));
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Type_Node& node, Context context)
    {
        if (const auto* const concrete = std::get_if<Concrete_Type>(&node.type)) {
            if (context == Context::return_type) {
                return_info = Return_Info { .type = *concrete, .token = node.token };
            }
            node.const_value = Value { *concrete };
            return {};
        }

        const auto& generic = std::get<Bit_Generic_Type>(node.type);

        if (auto r = analyze_types(generic.width, Context::constant_expression); !r) {
            return r;
        }
        auto width = get_const_value(get_node(generic.width));
        BIT_MANIPULATION_ASSERT(width.has_value());
        if (!width->type.is_integer()) {
            return Analysis_Error { Analysis_Error_Code::width_not_integer,
                                    get_token(get_node(generic.width)) };
        }
        const Big_Int folded_width = width->int_value.value();
        if (folded_width == 0) {
            return Analysis_Error { Analysis_Error_Code::width_zero,
                                    get_token(get_node(generic.width)) };
        }
        if (folded_width > uint_max_width) {
            return Analysis_Error { Analysis_Error_Code::width_too_large,
                                    get_token(get_node(generic.width)) };
        }
        const Concrete_Type result = Concrete_Type::Uint(static_cast<int>(folded_width));
        node.type = result;
        if (context == Context::return_type) {
            return_info = Return_Info { .type = result, .token = node.token };
        }
        node.const_value = Value { result };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Let_Const_Node& node, Context context)
    {
        const auto initializer_context
            = node.is_const ? Context::constant_expression : Context::regular;

        if (node.get_type() == Node_Handle::null) {
            // If there is no type, there must be an initializer.
            // This is "static type inference".
            // Prior analysis should have ensured this already, but we may a well double-check.
            BIT_MANIPULATION_ASSERT(node.get_initializer() != Node_Handle::null);

            if (auto r = analyze_types(node.get_initializer(), initializer_context); !r) {
                return r;
            }
            auto initializer_value = get_const_value(get_node(node.get_initializer()));
            BIT_MANIPULATION_ASSERT(initializer_value.has_value());
            if (node.is_const) {
                node.const_value = initializer_value;
            }
            else {
                // Intentionally "forget" the value determined during constant folding.
                // For mutable variables, it is possible that the value is modified, and
                // `const_value` should only be a concrete value for invariants.
                node.const_value = Value { initializer_value->type };
            }
            return {};
        }

        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        const auto type_result = analyze_types(type_node, context);
        if (!type_result) {
            return type_result;
        }
        if (node.get_initializer() == Node_Handle::null) {
            node.const_value = Value { std::get<Concrete_Type>(type_node.type) };
            return {};
        }

        if (const auto r = analyze_types(node.get_initializer(), initializer_context); !r) {
            return r;
        }
        auto initializer_value = get_const_value(get_node(node.get_initializer()));

        const Token cause_token = get_token(get_node(node.get_initializer()));
        BIT_MANIPULATION_ASSERT(type_node.const_value.has_value());
        if (!initializer_value->type.is_convertible_to(type_node.const_value->type)) {
            return Analysis_Error { Type_Error::incompatible_types, node.token, cause_token };
        }
        const Result<Value, Evaluation_Error> r
            = evaluate_conversion(*initializer_value, type_node.const_value->type);
        if (r) {
            node.const_value = node.is_const ? *r : Value { r->type };
            return {};
        }
        else if (node.is_const) {
            return Analysis_Error { r.error(), node.token, cause_token };
        }
        BIT_MANIPULATION_ASSERT(context != Context::constant_expression);
        // Even if evaluation failed, it is legal if we are in dead code.
        node.const_value = Value { type_node.const_value->type };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(If_Statement_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_condition(), context); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& if_block = std::get<Block_Statement_Node>(get_node(node.get_if_block()));
        auto if_result = analyze_types(if_block, context);
        if (!if_result) {
            return if_result;
        }
        if (auto else_result = analyze_types(node.get_else_block(), context); !else_result) {
            return else_result;
        }
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(While_Statement_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_condition(), context); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& block = std::get<Block_Statement_Node>(get_node(node.get_block()));
        node.const_value = Value::Void;
        return analyze_types(block, context);
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Jump_Node& node, Context)
    {
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Return_Statement_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_expression(), context); !r) {
            return r;
        }
        auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());
        if (!expr_value->type.is_convertible_to(return_info->type)) {
            return Analysis_Error { Type_Error::incompatible_types, node.token,
                                    return_info->token };
        }
        BIT_MANIPULATION_ASSERT(context != Context::constant_expression);
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_conversion(*expr_value, return_info->type);
        node.const_value = eval_result ? *eval_result : Value { return_info->type };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Assignment_Node& node, Context context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != Node_Handle::null);

        Some_Node& looked_up_node = get_node(node.lookup_result);
        if (const auto* const parameter = std::get_if<Parameter_Node>(&looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_parameter, node.token,
                                    get_token(looked_up_node) };
        }
        if (const auto* const function = std::get_if<Function_Node>(&looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_function, node.token,
                                    get_token(looked_up_node) };
        }

        auto& looked_up_var = std::get<Let_Const_Node>(looked_up_node);
        if (looked_up_var.is_const) {
            return Analysis_Error { Analysis_Error_Code::assigning_const, node.token,
                                    get_token(looked_up_node) };
        }

        if (auto r = analyze_types(node.get_expression(), context); !r) {
            return r;
        }
        auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Concrete_Type dest_type = looked_up_var.const_value.value().type;
        if (!expr_value->type.is_convertible_to(dest_type)) {
            return Analysis_Error { Type_Error::incompatible_types, node.token,
                                    get_token(get_node(node.get_expression())) };
        }

        BIT_MANIPULATION_ASSERT(context != Context::constant_expression);
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_conversion(*expr_value, dest_type);
        if (!eval_result) {
            node.const_value = Value { dest_type };
            return {};
        }
        node.const_value = *eval_result;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(If_Expression_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_condition(), context); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }
        Context left_context = context;
        Context right_context = context;
        // Similar to short-circuiting (see Binary_Expression_Node), we lower the analysis context
        // to `regular` for the expression that is not evaluated.
        // If we analyzed it as a constant expression, we would require a value from it, and
        // e.g. `1 if true else 0 / 0` would fail, even though it is valid and meant to select `1`.
        if (context == Context::constant_expression) {
            const bool evaluate_left = condition_value->int_value.value();
            (evaluate_left ? right_context : left_context) = Context::regular;
        }

        if (auto r = analyze_types(node.get_left(), left_context); !r) {
            return r;
        }
        auto left_value = get_const_value(get_node(node.get_left()));

        if (auto r = analyze_types(node.get_right(), right_context); !r) {
            return r;
        }
        auto right_value = get_const_value(get_node(node.get_right()));

        Result<Concrete_Type, Type_Error> type_result
            = check_if_expression(left_value->type, condition_value->type, right_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_if_expression(*left_value, *condition_value, *right_value);

        // Type analysis has already succeeded, and for context expressions, the condition is known.
        // Therefore, it should be impossible for an evaluation to fail.
        BIT_MANIPULATION_ASSERT(context != Context::constant_expression || eval_result.has_value());

        node.const_value = eval_result ? *eval_result : Value { *type_result };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Binary_Expression_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_left(), context); !r) {
            return r;
        }
        const auto& left_node = get_node(node.get_left());
        const auto left_value = get_const_value(left_node);

        {
            // For short-circuiting operators like && and ||, if we are in a constant expression,
            // we should not evaluate the rest as a constant expression if short-circuiting takes
            // place.
            //
            // For example, in `const x = true || 0 / 0 == 0;`, the initializer is correct
            // according to type analysis, and the division by zero is never executed.
            // Therefore, only type analysis should be applied upon short-circuiting, not
            // constant expression analysis, which would also demand a value from `0 / 0`.
            const bool is_short_circuiting
                = node.op == Token_Type::logical_and || node.op == Token_Type::logical_or;
            if (context == Context::constant_expression && is_short_circuiting) {
                if (left_value->type != Concrete_Type::Bool) {
                    return Analysis_Error { Type_Error::non_bool_logical, node.token,
                                            get_token(left_node) };
                }
                // This was analyzed as a constant expression, so a concrete value must have
                // emerged.
                BIT_MANIPULATION_ASSERT(left_value->int_value.has_value());
                const bool circuit_breaker = node.op == Token_Type::logical_or;
                if (*left_value->int_value == circuit_breaker) {
                    context = Context::regular;
                }
            }
        }

        if (auto r = analyze_types(node.get_right(), context); !r) {
            return r;
        }
        auto right_value = get_const_value(get_node(node.get_right()));

        const Result<Concrete_Type, Type_Error> type_result
            = check_binary_operator(left_value->type, node.op, right_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }

        const Result<Value, Evaluation_Error> eval_result
            = evaluate_binary_operator(*left_value, node.op, *right_value);
        if (!eval_result && context == Context::constant_expression) {
            return Analysis_Error { eval_result.error(), node.token };
        }

        node.const_value = eval_result ? *eval_result : Value { *type_result };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Prefix_Expression_Node& node, Context context)
    {
        if (auto r = analyze_types(node.get_expression(), context); !r) {
            return r;
        }
        const auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Result<Concrete_Type, Type_Error> type_result
            = check_unary_operator(node.op, expr_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_unary_operator(node.op, *expr_value);
        if (!eval_result && context == Context::constant_expression) {
            return Analysis_Error { eval_result.error(), node.token };
        }
        node.const_value = eval_result ? *eval_result : Value { *type_result };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Function_Call_Expression_Node& node, Context context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != Node_Handle::null);

        // 1. Verify that we call a function.

        Some_Node& looked_up = get_node(node.lookup_result);
        auto* const function = std::get_if<Function_Node>(&looked_up);
        if (!function) {
            return Analysis_Error { Analysis_Error_Code::call_non_function, node.token,
                                    get_token(looked_up) };
        }

        // 2. Verify that we have the right number of arguments.

        auto& params = std::get<Parameter_List_Node>(get_node(function->get_parameters()));
        if (params.parameters.size() != node.arguments.size()) {
            return Analysis_Error { Analysis_Error_Code::wrong_number_of_arguments, node.token,
                                    function->token };
        }

        // 3. Identify bit-generic parameters and perform implicit instantiation based on the
        //    bit width of the corresponding arguments.

        // TODO: do this

        // 4. Perform analysis of the called function.
        //    Except for constant expressions, this can happen in a limited capacity.

        const Context inner_context = context == Context::constant_expression
            ? Context::constant_expression
            : Context::function_call;

        std::optional<Concrete_Type> return_type;

        if (!function->is_generic) {
            if (!function->const_value) {
                auto r = analyze_types(*function, inner_context);
                if (!r) {
                    return r;
                }
            }
            BIT_MANIPULATION_ASSERT(function->const_value);
            return_type = function->const_value->type;
        }

        // 5. Check whether the function can be called with the given arguments and obtain
        //    concrete values.

        std::vector<Value> arg_values;
        if (context == Context::constant_expression) {
            arg_values.reserve(node.arguments.size());
        }

        for (Size i = 0; i < node.arguments.size(); ++i) {
            const Node_Handle arg = node.arguments[i];

            if (auto r = analyze_types(arg, context); !r) {
                return r;
            }
            const auto arg_value = get_const_value(get_node(arg));
            auto& param = std::get<Parameter_Node>(get_node(params.parameters[i]));
            const Concrete_Type param_type = param.const_value.value().type;
            if (!arg_value->type.is_convertible_to(param_type)) {
                return Analysis_Error { Type_Error::incompatible_types, node.token, param.token };
            }

            Result<Value, Evaluation_Error> conv_result
                = evaluate_conversion(*arg_value, param_type);
            // FIXME: this code is probably borked
            if (context == Context::constant_expression && !conv_result) {
                return Analysis_Error { conv_result.error(), node.token, param.token };
            }
            if (context == Context::constant_expression) {
                arg_values.push_back(*conv_result);
            }
        }

        // 6. If need be, perform constant evaluation of the function call.
        // TODO:

        // 7. Return results

        node.const_value = Value { return_type.value() };
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Id_Expression_Node& node, Context context)
    {
        if (node.bit_generic) {
            // instantiation should have assigned a value to all bit-generic id-expressions
            BIT_MANIPULATION_ASSERT(node.const_value);
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.lookup_result != Node_Handle::null);

        Some_Node& looked_up_node = get_node(node.lookup_result);
        if (const auto* const looked_up_function = std::get_if<Function_Node>(&looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::function_in_expression, node.token,
                                    looked_up_function->token };
        }
        if (const auto* const looked_up_var = std::get_if<Let_Const_Node>(&looked_up_node)) {
            if (context == Context::constant_expression && !looked_up_var->is_const) {
                return Analysis_Error { Analysis_Error_Code::let_variable_in_constant_expression,
                                        node.token, looked_up_var->token };
            }
            if (looked_up_var->const_value) {
                node.const_value = looked_up_var->const_value;
                return {};
            }
        }
        if (const auto* const looked_up_param = std::get_if<Parameter_Node>(&looked_up_node)) {
            if (context == Context::constant_expression) {
                return Analysis_Error { Analysis_Error_Code::parameter_in_constant_expression,
                                        node.token, looked_up_param->token };
            }
        }

        if (auto r = analyze_types(node.lookup_result, context); !r) {
            return r;
        }
        const auto lookup_value = get_const_value(looked_up_node);
        BIT_MANIPULATION_ASSERT(lookup_value.has_value());
        if (context == Context::constant_expression && lookup_value->is_unknown()) {
            return Analysis_Error { Analysis_Error_Code::expected_constant_expression, node.token,
                                    get_token(looked_up_node) };
        }
        node.const_value = lookup_value;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Literal_Node& node, Context)
    {
        switch (node.token.type) {
        case Token_Type::keyword_bool: {
            node.const_value = Value::True;
            return {};
        }
        case Token_Type::keyword_false: {
            node.const_value = Value::False;
            return {};
        }
        case Token_Type::binary_literal:
        case Token_Type::octal_literal:
        case Token_Type::decimal_literal:
        case Token_Type::hexadecimal_literal: {
            std::optional<Big_Int> value
                = parse_integer_literal(node.token.extract(m_program.source));
            if (!value) {
                return Analysis_Error { Analysis_Error_Code::invalid_integer_literal, node.token };
            }
            const auto result = Value { Concrete_Type::Int, *value };
            node.const_value = result;
            return {};
        }
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }
};

} // namespace

Result<void, Analysis_Error> analyze(Parsed_Program& program)
{
    if (auto r = analyze_name_lookup(program); !r) {
        return r;
    }
    else {
        return r;
    }
}

} // namespace bit_manipulation::bms