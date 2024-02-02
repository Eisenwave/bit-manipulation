#include <unordered_map>

#include "bms/analyze.hpp"
#include "bms/bms.hpp"
#include "bms/operations.hpp"
#include "bms/parse_number.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

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
        auto result = analyze_types(m_program.root_node, Context::regular);
        if (result) {
            return {};
        }
        else {
            return result.error();
        }
    }

private:
    Result<Value, Analysis_Error> analyze_types(Node_Handle handle, Context context)
    {
        if (handle == Node_Handle::null) {
            return Value::Void;
        }
        return std::visit([this, context](auto& node) { return analyze_types(node, context); },
                          get_node(handle));
    }

    template <typename T>
    Result<Value, Analysis_Error> analyze_types(T& node, Context context)
    {
        for (Node_Handle h : node.get_children()) {
            auto r = analyze_types(h, context);
            if (!r) {
                return r;
            }
        }
        return Value::Void;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Function_Node& node, Context context)
    {
        if (node.is_generic) {
            for (Node_Handle instance : node.instances) {
                auto r = analyze_types(instance, context);
                if (!r) {
                    return r;
                }
            }
            return Value::Void;
        }
        if (auto r = analyze_types(node.get_parameters(), context); !r) {
            return r;
        }
        auto return_result = analyze_types(node.get_return_type(), Context::return_type);
        if (!return_result) {
            return return_result;
        }
        if (auto r = analyze_types(node.get_requires_clause(), Context::constant_expression); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_body(), context); !r) {
            return r;
        }
        node.const_value = *return_result;
        return Value::Void;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Type_Node& node, Context context)
    {
        if (const auto* const concrete = std::get_if<Concrete_Type>(&node.type)) {
            if (context == Context::return_type) {
                return_info = Return_Info { .type = *concrete, .token = node.token };
            }
            return Value { *concrete };
        }

        const auto& generic = std::get<Bit_Generic_Type>(node.type);

        auto r = analyze_types(generic.width, Context::constant_expression);
        if (!r) {
            return r;
        }
        if (!r->type.is_integer()) {
            return Analysis_Error { Analysis_Error_Code::width_not_integer,
                                    get_token(get_node(generic.width)) };
        }
        const Big_Int folded_width = r->int_value.value();
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
        return Value { result };
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Let_Const_Node& node, Context context)
    {
        const auto initializer_context
            = node.is_const ? Context::constant_expression : Context::regular;

        if (node.get_type() == Node_Handle::null) {
            // If there is no type, there must be an initializer.
            // This is "static type inference".
            // Prior analysis should have ensured this already, but we may a well double-check.
            BIT_MANIPULATION_ASSERT(node.get_initializer() != Node_Handle::null);

            const auto initializer_result
                = analyze_types(node.get_initializer(), initializer_context);
            if (!initializer_result) {
                return initializer_result;
            }
            node.const_value = *initializer_result;
            return node.const_value.value();
        }

        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        const auto type_result = analyze_types(type_node, context);
        if (!type_result) {
            return type_result;
        }
        if (node.get_initializer() == Node_Handle::null) {
            node.const_value = Value { std::get<Concrete_Type>(type_node.type) };
            return *node.const_value;
        }
        const auto initializer_result = analyze_types(node.get_initializer(), initializer_context);
        if (!initializer_result) {
            return initializer_result;
        }

        const Result<Value, Evaluation_Error> r
            = evaluate_conversion(*initializer_result, type_result->type);
        if (r) {
            node.const_value = *r;
            return *r;
        }
        const Token cause_token = get_token(get_node(node.get_initializer()));

        return Analysis_Error { r.error(), node.token, cause_token };
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(If_Statement_Node& node, Context context)
    {
        auto r = analyze_types(node.get_condition(), context);
        if (!r) {
            return r;
        }
        if (r->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& if_block = std::get<Block_Statement_Node>(get_node(node.get_if_block()));
        auto if_result = analyze_types(if_block, context);
        if (!if_result) {
            return if_result;
        }
        return analyze_types(node.get_else_block(), context);
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(While_Statement_Node& node, Context context)
    {
        auto r = analyze_types(node.get_condition(), context);
        if (!r) {
            return r;
        }
        if (r->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& block = std::get<Block_Statement_Node>(get_node(node.get_block()));
        return analyze_types(block, context);
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Jump_Node&, Context)
    {
        return Value::Void;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Return_Statement_Node& node, Context context)
    {
        auto r = analyze_types(node.get_expression(), context);
        if (!r) {
            return r;
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_conversion(*r, return_info->type);
        if (!eval_result) {
            return Analysis_Error { eval_result.error(), node.token, return_info->token };
        }
        node.const_value = *eval_result;
        return *eval_result;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Assignment_Node& node, Context context)
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

        auto r = analyze_types(node.get_expression(), context);
        if (!r) {
            return r;
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_conversion(*r, looked_up_var.const_value.value().type);
        if (!eval_result) {
            return Analysis_Error { eval_result.error(), node.token, return_info->token };
        }
        node.const_value = *eval_result;
        return *eval_result;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(If_Expression_Node& node, Context context)
    {
        auto left_result = analyze_types(node.get_left(), context);
        if (!left_result) {
            return left_result;
        }
        auto condition_result = analyze_types(node.get_condition(), context);
        if (!condition_result) {
            return condition_result;
        }
        if (condition_result->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto right_result = analyze_types(node.get_right(), context);
        if (!right_result) {
            return right_result;
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_if_expression(*left_result, *condition_result, *right_result);
        if (!eval_result) {
            return Analysis_Error { eval_result.error(), node.token };
        }
        return *eval_result;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Binary_Expression_Node& node, Context context)
    {
        auto left_result = analyze_types(node.get_left(), context);
        if (!left_result) {
            return left_result;
        }
        // TODO: short circuiting for && and ||
        auto right_result = analyze_types(node.get_right(), context);
        if (!right_result) {
            return right_result;
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_binary_operator(*left_result, node.op, *right_result);

        if (!eval_result) {
            return Analysis_Error { eval_result.error(), node.token };
        }
        return *eval_result;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Prefix_Expression_Node& node, Context context)
    {
        auto expression_result = analyze_types(node.get_expression(), context);
        if (!expression_result) {
            return expression_result;
        }
        const Result<Value, Evaluation_Error> eval_result
            = evaluate_unary_operator(node.op, *expression_result);

        if (!eval_result) {
            return Analysis_Error { eval_result.error(), node.token };
        }
        return *eval_result;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Function_Call_Expression_Node& node,
                                                Context context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != Node_Handle::null);

        // 1. Verify that we call a function.

        Some_Node& looked_up = get_node(node.lookup_result);
        const auto* const function = std::get_if<Function_Node>(&looked_up);
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
            Node_Handle arg = node.arguments[i];
            auto arg_result = analyze_types(arg, context);
            if (!arg_result) {
                return arg_result;
            }
            auto& param = std::get<Parameter_Node>(get_node(params.parameters[i]));
            const Concrete_Type param_type = param.const_value.value().type;
            auto conv_result = evaluate_conversion(*arg_result, param_type);
            if (!conv_result) {
                return Analysis_Error { conv_result.error(), node.token, param.token };
            }
            if (context == Context::constant_expression) {
                arg_values.push_back(*conv_result);
            }
        }

        // 6. If need be, perform constant evaluation of the function call.
        // TODO:

        // 7. Return results

        return Value { return_type.value() };
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Id_Expression_Node& node, Context context)
    {
        if (node.bit_generic) {
            // instantiation should have assigned a value to all bit-generic id-expressions
            BIT_MANIPULATION_ASSERT(node.const_value);
            return *node.const_value;
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
                return *looked_up_var->const_value;
            }
        }
        if (const auto* const looked_up_param = std::get_if<Parameter_Node>(&looked_up_node)) {
            if (context == Context::constant_expression) {
                return Analysis_Error { Analysis_Error_Code::parameter_in_constant_expression,
                                        node.token, looked_up_param->token };
            }
        }

        auto r = analyze_types(node.lookup_result, context);
        if (!r) {
            return r;
        }
        if (r->is_unknown() && context == Context::constant_expression) {
            return Analysis_Error { Analysis_Error_Code::expected_constant_expression, node.token,
                                    get_token(looked_up_node) };
        }
        node.const_value = *r;
        return *r;
    }

    template <>
    Result<Value, Analysis_Error> analyze_types(Literal_Node& node, Context)
    {
        switch (node.token.type) {
        case Token_Type::keyword_bool: {
            node.const_value = Value::True;
            return Value::True;
        }
        case Token_Type::keyword_false: {
            node.const_value = Value::False;
            return Value::False;
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
            return result;
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