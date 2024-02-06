#include <unordered_map>

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"
#include "bms/parse_number.hpp"
#include "bms/vm.hpp"
#include "bms/vm_codegen.hpp"

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

template <typename T>
concept Lookup_Performing_Node = requires(T& t) {
    {
        t.lookup_result
    } -> std::same_as<Node_Handle&>;
};

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

    Result<void, Analysis_Error> instantiate_all(const Widths& w)
    {
        for (Node_Handle decl : m_root.declarations) {
            if (auto* f = std::get_if<Function_Node>(&get_node(decl))) {
                auto r = instantiate_function(decl, *f, w);
                if (!r) {
                    return r.error();
                }
            }
        }
        return {};
    }

    Result<const Function_Node::Instance*, Analysis_Error>
    instantiate_function(Node_Handle h, Function_Node& node, const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(node.is_generic);

        if (const Function_Node::Instance* const existing = node.find_instance(w)) {
            return existing;
        }

        Node_Handle instance = deep_copy(h);
        auto& instance_node = std::get<Function_Node>(get_node(instance));
        auto effective_widths = substitute_widths(instance_node, w);
        if (!effective_widths) {
            return effective_widths.error();
        }
        auto& new_instance = node.instances.emplace_back(std::move(*effective_widths), instance);
        return &new_instance;
    }

private:
    Result<std::vector<int>, Analysis_Error> substitute_widths(Function_Node& instance,
                                                               const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(!instance.is_generic);

        std::vector<int> widths;
        const auto next_width = [&widths, &w] {
            const int result = get_width(w, widths.size());
            widths.push_back(result);
            return result;
        };

        auto& params = std::get<Parameter_List_Node>(get_node(instance.get_parameters()));
        for (Node_Handle p : params.parameters) {
            auto& param_node = std::get<Parameter_Node>(get_node(p));
            auto& type_node = std::get<Type_Node>(get_node(param_node.get_type()));
            auto* generic_type = std::get_if<Bit_Generic_Type>(&type_node.type);
            if (!generic_type) {
                continue;
            }
            BIT_MANIPULATION_ASSERT(generic_type->type == Type_Type::Uint);
            auto* id_node = std::get_if<Id_Expression_Node>(&get_node(generic_type->width));
            // If it isn't an id-expression, it cannot be a generic parameter.
            if (id_node && id_node->bit_generic) {
                const int width = next_width();
                // Merely replacing the type isn't enough; we must also give this node a
                // constant value so that existing name lookup can obtain this value.
                id_node->const_value = Value::Int(width);
                type_node.type = Concrete_Type::Uint(width);
            }
        }

        return widths;
    }

    Node_Handle deep_copy(Node_Handle h)
    {
        // Two passes are necessary.
        // During the first pass, the nodes are all copied and the links to children are updated
        // in the copies.
        // However, this does not yet affect the name lookup results, which must also refer to
        // the cloned nodes.
        // This is done in a second pass, since cloning a cyclic graph in one go is difficult.
        std::unordered_map<Node_Handle, Node_Handle> remap;
        const Node_Handle result = deep_copy_for_instantiation(h, remap);
        deep_update_name_lookup(h, remap);

        return result;
    }

    Node_Handle deep_copy_for_instantiation(Node_Handle h,
                                            std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        const Node_Handle result = copy_single_node_for_instantiation(h);
        remap.emplace(h, result);

        for (Node_Handle& child : get_children(m_program.get_node(result))) {
            child = deep_copy_for_instantiation(child, remap);
        }
        return result;
    }

    void deep_update_name_lookup(Node_Handle h,
                                 const std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        auto& node = get_node(h);
        std::visit(
            [&h, &remap]<typename T>(T& n) {
                if constexpr (Lookup_Performing_Node<T>) {
                    n.lookup_result = remap.at(h);
                }
            },
            node);
        for (Node_Handle child : get_children(node)) {
            deep_update_name_lookup(child, remap);
        }
    }

    Node_Handle copy_single_node_for_instantiation(Node_Handle h)
    {
        return std::visit(
            [this]<typename T>(const T& n) {
                if constexpr (std::is_same_v<T, Function_Node>) {
                    return m_program.push_node(n.copy_for_instantiation());
                }
                else {
                    return m_program.push_node(T(n));
                }
            },
            m_program.get_node(h));
    }
};

// =================================================================================================

enum struct Expression_Context : Default_Underlying {
    // The default context. Full analysis of functions and everything inside.
    normal,
    // A constant expression. Constant evaluation cannot run into an unknown value.
    constant,
};

struct Type_Analyzer : Analyzer_Base {
private:
    struct Return_Info {
        Concrete_Type type;
        Token token;
    };
    Virtual_Machine constant_evaluation_machine;
    std::optional<Return_Info> return_info;

public:
    Type_Analyzer(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        return analyze_types(m_program.root_node, Analysis_Level::full, Expression_Context::normal);
    }

private:
    Result<void, Analysis_Error>
    analyze_types(Node_Handle handle, Analysis_Level level, Expression_Context context)
    {
        if (handle == Node_Handle::null) {
            return {};
        }
        return std::visit(
            [this, level, context](auto& node) { return analyze_types(node, level, context); },
            get_node(handle));
    }

    template <typename T>
    Result<void, Analysis_Error> analyze_types(T& node, Analysis_Level, Expression_Context)
        = delete;

    template <typename T>
        requires(!std::is_const_v<T>)
    Result<void, Analysis_Error>
    analyze_child_types(T& node, Analysis_Level level, Expression_Context context)
    {
        for (Node_Handle h : node.get_children()) {
            auto r = analyze_types(h, level, context);
            if (!r) {
                return r;
            }
        }
        node.const_value = Value::Void;
        return {};
    }

    template <>
    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(Program_Node& node, Analysis_Level level, Expression_Context)
    {
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Function_Node& node, Analysis_Level level, Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(level != Analysis_Level::unanalyzed);
        if (node.analysis_so_far >= level) {
            return {};
        }

        // If the function is generic, we can never say that analysis succeeded entirely because
        // new implicit instantiations may have been generated in the meantime.
        // For concrete functions, we can memoize the result of type analysis.
        if (node.is_generic) {
            for (const Function_Node::Instance& instance : node.instances) {
                auto r = analyze_types(instance.handle, level, Expression_Context::normal);
                if (!r) {
                    return r;
                }
            }
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.instances.empty());

        if (auto r = analyze_types(node.get_parameters(), level, Expression_Context::normal); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_return_type(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto& return_type = std::get<Type_Node>(get_node(node.get_return_type()));
        return_info
            = Return_Info { .type = return_type.const_value->type, .token = return_type.token };

        if (node.get_requires_clause() != Node_Handle::null) {
            auto& expression_node = get_node(node.get_requires_clause());
            auto& expr_const_value = get_const_value(expression_node);
            // Expressions in requires-clauses don't need to be checked twice, but we have to
            // determine this here instead of in the expression because expressions lack a
            // checking mechanism for themselves.
            if (!expr_const_value) {
                if (auto r = analyze_types(node.get_requires_clause(), Analysis_Level::deep,
                                           Expression_Context::constant);
                    !r) {
                    return r;
                }
                node.const_value = expr_const_value;
                BIT_MANIPULATION_ASSERT(node.const_value && node.const_value->int_value);

                if (node.const_value->type != Concrete_Type::Bool) {
                    return Analysis_Error { Analysis_Error_Code::requires_clause_not_bool,
                                            node.token, get_token(expression_node) };
                }
                if (node.const_value->int_value != 1) {
                    return Analysis_Error { Analysis_Error_Code::requires_clause_not_satisfied,
                                            node.token, get_token(expression_node) };
                }
            }
        }

        if (node.analysis_so_far < Analysis_Level::shallow) {
            node.analysis_so_far = Analysis_Level::shallow;
        }
        if (level >= Analysis_Level::full) {
            if (auto r = analyze_types(node.get_body(), level, Expression_Context::normal); !r) {
                return r;
            }
        }
        if (level >= Analysis_Level::deep) {
            const Size vm_address = constant_evaluation_machine.instruction_count();
            Result<void, Analysis_Error> instructions
                = generate_code(constant_evaluation_machine.instructions(), m_program, node);
            if (!instructions) {
                return instructions.error();
            }
            node.vm_address = vm_address;
        }
        BIT_MANIPULATION_ASSERT(!node.is_generic);
        node.const_value = get_const_value(get_node(node.get_return_type()));
        node.analysis_so_far = level;
        return {};
    }

    template <>
    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(Parameter_List_Node& node, Analysis_Level level, Expression_Context)
    {
        if (node.const_value) {
            return {};
        }
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    template <>
    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(Parameter_Node& node, Analysis_Level level, Expression_Context)
    {
        if (node.const_value) {
            return {};
        }
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Type_Node& node, Analysis_Level, Expression_Context)
    {
        if (node.const_value) {
            return {};
        }
        if (const auto* const concrete = std::get_if<Concrete_Type>(&node.type)) {
            node.const_value = Value::unknown_of_type(*concrete);
            return {};
        }

        const auto& generic = std::get<Bit_Generic_Type>(node.type);

        if (auto r
            = analyze_types(generic.width, Analysis_Level::deep, Expression_Context::constant);
            !r) {
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
        node.const_value = Value::unknown_of_type(result);
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Const_Node& node, Analysis_Level level, Expression_Context)
    {
        // Const nodes contain constant expressions, so if they have been analyzed in the past,
        // we can be sure that no further analysis is required.
        if (node.const_value) {
            return {};
        }
        // Const nodes must always have an initializer.
        BIT_MANIPULATION_ASSERT(node.get_initializer() != Node_Handle::null);

        if (node.get_type() == Node_Handle::null) {
            if (auto r = analyze_types(node.get_initializer(), Analysis_Level::deep,
                                       Expression_Context::constant);
                !r) {
                return r;
            }
            node.const_value = get_const_value(get_node(node.get_initializer()));
            BIT_MANIPULATION_ASSERT(node.const_value->int_value);
            return {};
        }

        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        const auto type_result = analyze_types(type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }

        if (const auto r = analyze_types(node.get_initializer(), Analysis_Level::deep,
                                         Expression_Context::constant);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(get_node(node.get_initializer()));

        const Token cause_token = get_token(get_node(node.get_initializer()));
        BIT_MANIPULATION_ASSERT(type_node.const_value.has_value());
        if (!initializer_value->type.is_convertible_to(type_node.const_value->type)) {
            return Analysis_Error { Type_Error_Code::incompatible_types, node.token, cause_token };
        }
        const Result<Value, Evaluation_Error_Code> r
            = evaluate_conversion(*initializer_value, type_node.const_value->type);
        if (!r) {
            return Analysis_Error { r.error(), node.token, cause_token };
        }
        node.const_value = *r;
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Let_Node& node, Analysis_Level level, Expression_Context)
    {
        if (node.get_type() == Node_Handle::null) {
            // If there is no type, there must be an initializer.
            // This is "static type inference".
            // Prior analysis should have ensured this already, but we may a well double-check.
            BIT_MANIPULATION_ASSERT(node.get_initializer() != Node_Handle::null);

            if (auto r = analyze_types(node.get_initializer(), level, Expression_Context::normal);
                !r) {
                return r;
            }
            auto initializer_value = get_const_value(get_node(node.get_initializer()));
            BIT_MANIPULATION_ASSERT(initializer_value.has_value());
            // Intentionally "forget" the value determined during constant folding.
            // For mutable variables, it is possible that the value is modified, and
            // `const_value` should only be a concrete value for invariants.
            node.const_value = Value::unknown_of_type(initializer_value->type);
            return {};
        }

        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        const auto type_result = analyze_types(type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }
        auto& type_node_type = std::get<Concrete_Type>(type_node.type);
        if (node.get_initializer() == Node_Handle::null) {
            node.const_value = Value::unknown_of_type(type_node_type);
            return {};
        }

        if (const auto r = analyze_types(node.get_initializer(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(get_node(node.get_initializer()));

        const Token cause_token = get_token(get_node(node.get_initializer()));
        BIT_MANIPULATION_ASSERT(type_node.const_value.has_value());
        if (!initializer_value->type.is_convertible_to(type_node.const_value->type)) {
            return Analysis_Error { Type_Error_Code::incompatible_types, node.token, cause_token };
        }
        node.const_value = Value::unknown_of_type(type_node_type);
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Static_Assert_Node& node, Analysis_Level, Expression_Context)
    {
        // Static assertions never need to be checked twice.
        if (node.const_value) {
            BIT_MANIPULATION_ASSERT(node.const_value->int_value);
            return {};
        }
        auto expression_result = analyze_types(node.get_expression(), Analysis_Level::deep,
                                               Expression_Context::constant);
        if (!expression_result) {
            return expression_result;
        }
        auto& expression_node = get_node(node.get_expression());
        node.const_value = get_const_value(expression_node);
        BIT_MANIPULATION_ASSERT(node.const_value && node.const_value->int_value);

        if (node.const_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::static_assert_expression_not_bool,
                                    node.token, get_token(expression_node) };
        }
        if (node.const_value->int_value != 1) {
            return Analysis_Error { Analysis_Error_Code::static_assertion_failed, node.token,
                                    get_token(expression_node) };
        }

        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(If_Statement_Node& node, Analysis_Level level, Expression_Context)
    {
        if (auto r = analyze_types(node.get_condition(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& if_block = std::get<Block_Statement_Node>(get_node(node.get_if_block()));
        auto if_result = analyze_types(if_block, level, Expression_Context::normal);
        if (!if_result) {
            return if_result;
        }
        if (auto else_result
            = analyze_types(node.get_else_block(), level, Expression_Context::normal);
            !else_result) {
            return else_result;
        }
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(While_Statement_Node& node, Analysis_Level level, Expression_Context)
    {
        if (auto r = analyze_types(node.get_condition(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }

        auto& block = std::get<Block_Statement_Node>(get_node(node.get_block()));
        if (auto r = analyze_types(block, level, Expression_Context::normal); !r) {
            return r;
        }
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Jump_Node& node, Analysis_Level, Expression_Context)
    {
        node.const_value = Value::Void;
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Return_Statement_Node& node, Analysis_Level level, Expression_Context)
    {
        if (auto r = analyze_types(node.get_expression(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());
        if (!expr_value->type.is_convertible_to(return_info->type)) {
            return Analysis_Error { Type_Error_Code::incompatible_types, node.token,
                                    return_info->token };
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_conversion(*expr_value, return_info->type);
        node.const_value = eval_result ? *eval_result : Value::unknown_of_type(return_info->type);
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Assignment_Node& node, Analysis_Level level, Expression_Context)
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

        if (auto* looked_up_const = std::get_if<Const_Node>(&looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_const, node.token,
                                    get_token(*looked_up_const) };
        }

        auto& looked_up_var = std::get<Let_Node>(looked_up_node);

        if (auto r = analyze_types(node.get_expression(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Concrete_Type dest_type = looked_up_var.const_value.value().type;
        if (!expr_value->type.is_convertible_to(dest_type)) {
            return Analysis_Error { Type_Error_Code::incompatible_types, node.token,
                                    get_token(get_node(node.get_expression())) };
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_conversion(*expr_value, dest_type);
        if (!eval_result) {
            node.const_value = Value::unknown_of_type(dest_type);
            return {};
        }
        node.const_value = *eval_result;
        return {};
    }

    template <>
    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(Block_Statement_Node& node, Analysis_Level level, Expression_Context)
    {
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(If_Expression_Node& node, Analysis_Level level, Expression_Context context)
    {
        if (auto r = analyze_types(node.get_condition(), level, context); !r) {
            return r;
        }
        auto condition_value = get_const_value(get_node(node.get_condition()));
        if (condition_value->type != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, node.token,
                                    get_token(get_node(node.get_condition())) };
        }
        Expression_Context left_context = context;
        Expression_Context right_context = context;
        // Similar to short-circuiting (see Binary_Expression_Node), we lower the analysis context
        // to `normal` for the expression that is not evaluated.
        // If we analyzed it as a constant expression, we would require a value from it, and
        // e.g. `1 if true else 0 / 0` would fail, even though it is valid and meant to select `1`.
        if (context == Expression_Context::constant) {
            const bool evaluate_left = condition_value->int_value.value();
            (evaluate_left ? right_context : left_context) = Expression_Context::normal;
        }

        if (auto r = analyze_types(node.get_left(), level, left_context); !r) {
            return r;
        }
        auto left_value = get_const_value(get_node(node.get_left()));

        if (auto r = analyze_types(node.get_right(), level, right_context); !r) {
            return r;
        }
        auto right_value = get_const_value(get_node(node.get_right()));

        Result<Concrete_Type, Type_Error_Code> type_result
            = check_if_expression(left_value->type, condition_value->type, right_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_if_expression(*left_value, *condition_value, *right_value);

        // Type analysis has already succeeded, and for context expressions, the condition is known.
        // Therefore, it should be impossible for an evaluation to fail.
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant || eval_result.has_value());

        node.const_value = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Binary_Expression_Node& node, Analysis_Level level, Expression_Context context)
    {
        if (auto r = analyze_types(node.get_left(), level, context); !r) {
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
            if (context == Expression_Context::constant && is_short_circuiting) {
                if (left_value->type != Concrete_Type::Bool) {
                    return Analysis_Error { Type_Error_Code::non_bool_logical, node.token,
                                            get_token(left_node) };
                }
                // This was analyzed as a constant expression, so a concrete value must have
                // emerged.
                BIT_MANIPULATION_ASSERT(left_value->int_value.has_value());
                const bool circuit_breaker = node.op == Token_Type::logical_or;
                if (*left_value->int_value == circuit_breaker) {
                    context = Expression_Context::normal;
                }
            }
        }

        if (auto r = analyze_types(node.get_right(), level, context); !r) {
            return r;
        }
        auto right_value = get_const_value(get_node(node.get_right()));

        const Result<Concrete_Type, Type_Error_Code> type_result
            = check_binary_operator(left_value->type, node.op, right_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_binary_operator(*left_value, node.op, *right_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error { eval_result.error(), node.token };
        }

        node.const_value = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Prefix_Expression_Node& node, Analysis_Level level, Expression_Context context)
    {
        if (auto r = analyze_types(node.get_expression(), level, context); !r) {
            return r;
        }
        const auto expr_value = get_const_value(get_node(node.get_expression()));
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Result<Concrete_Type, Type_Error_Code> type_result
            = check_unary_operator(node.op, expr_value->type);
        if (!type_result) {
            return Analysis_Error { type_result.error(), node.token };
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_unary_operator(node.op, *expr_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error { eval_result.error(), node.token };
        }
        node.const_value = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_types(Function_Call_Expression_Node& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != Node_Handle::null);

        // 1. Evaluate arguments.

        std::vector<Value> arg_values;
        arg_values.reserve(node.arguments.size());

        for (const Node_Handle arg : node.arguments) {
            if (auto r = analyze_types(arg, level, context); !r) {
                return r;
            }
            arg_values.push_back(get_const_value(get_node(arg)).value());
        }

        // 2. Verify that we call a function.

        Some_Node& looked_up = get_node(node.lookup_result);
        auto* function = std::get_if<Function_Node>(&looked_up);
        if (!function) {
            return Analysis_Error { Analysis_Error_Code::call_non_function, node.token,
                                    get_token(looked_up) };
        }

        // 3. Verify that we have the right number of arguments.
        //    We can perform this test prior to deduction because calling a function with the wrong
        //    number of arguments is an error before and after instantiating generic functions.
        //    There is no function overloading.

        auto& possibly_generic_params
            = std::get<Parameter_List_Node>(get_node(function->get_parameters()));
        if (possibly_generic_params.parameters.size() != node.arguments.size()) {
            return Analysis_Error { Analysis_Error_Code::wrong_number_of_arguments, node.token,
                                    function->token };
        }

        // 4. If necessary, perform bit-generic argument deduction.
        //    After deduction is complete, `function` will point to the concrete instance, not to
        //    the generic function anymore.
        if (function->is_generic) {
            std::vector<int> deduced_widths;
            for (Size i = 0; i < node.arguments.size(); ++i) {
                auto& param
                    = std::get<Parameter_Node>(get_node(possibly_generic_params.parameters[i]));
                auto& type = std::get<Type_Node>(get_node(param.get_type()));
                auto* gen_type = std::get_if<Bit_Generic_Type>(&type.type);
                if (!gen_type) {
                    continue;
                }
                auto* gen_expr = std::get_if<Id_Expression_Node>(&get_node(gen_type->width));
                if (!gen_expr || !gen_expr->bit_generic) {
                    continue;
                }
                if (!arg_values[i].type.is_uint()) {
                    return Analysis_Error { Analysis_Error_Code::width_deduction_from_non_uint,
                                            get_token(get_node(node.arguments[i])),
                                            gen_expr->token };
                }
                deduced_widths.push_back(arg_values[i].type.width());
            }

            Result<const Function_Node::Instance*, Analysis_Error> instantiation_result
                = Instantiator { m_program }.instantiate_function(node.lookup_result, *function,
                                                                  deduced_widths);
            if (!instantiation_result) {
                return instantiation_result.error();
            }
            function = &std::get<Function_Node>(get_node((*instantiation_result)->handle));
        }

        // 5. Analyze the called concrete function.
        //    If we are in a constant expression, the whole function has to be immediately analyzed
        //    as a constant expression.
        //    Otherwise, we don't need to analyze the function body immediately.
        BIT_MANIPULATION_ASSERT(!function->is_generic);

        const auto inner_level
            = context == Expression_Context::constant || level == Analysis_Level::deep
            ? Analysis_Level::deep
            : Analysis_Level::shallow;

        {
            // What we do here is akin to using a caller-saved register.
            // We would need a whole stack of return infos otherwise.
            auto preserved_return_info = return_info;
            auto r = analyze_types(*function, inner_level, context);
            if (!r) {
                return r;
            }
            return_info = preserved_return_info;
        }

        BIT_MANIPULATION_ASSERT(function->analysis_so_far >= inner_level);
        Concrete_Type return_type = function->const_value->type;

        // 6. Check whether the function can be called with the given arguments and obtain
        //    concrete values for the parameters if need be.

        auto& params = std::get<Parameter_List_Node>(get_node(function->get_parameters()));
        // Instantiation (which may have happened) should not be able to change the number of
        // parameters.
        BIT_MANIPULATION_ASSERT(params.parameters.size()
                                == possibly_generic_params.parameters.size());

        // Similar to `arg_values` above, but with implicit conversions applied for the purpose
        // of evaluating the actual function call.
        if (context == Expression_Context::constant) {
            constant_evaluation_machine.reset();
        }

        for (Size i = 0; i < node.arguments.size(); ++i) {
            auto& param = std::get<Parameter_Node>(get_node(params.parameters[i]));
            const Concrete_Type param_type = param.const_value.value().type;
            if (arg_values[i].type.is_convertible_to(param_type)) {
                return Analysis_Error { Type_Error_Code::incompatible_types, node.token,
                                        param.token };
            }

            Result<Value, Evaluation_Error_Code> conv_result
                = evaluate_conversion(arg_values[i], param_type);
            if (context == Expression_Context::constant) {
                if (!conv_result) {
                    return Analysis_Error { conv_result.error(), node.token, param.token };
                }
                constant_evaluation_machine.push(conv_result->concrete_value());
            }
        }

        // 7. Constant-evaluate the function call.
        if (context == Expression_Context::constant) {
            Result<void, Execution_Error_Code> cycle_result;
            while ((cycle_result = constant_evaluation_machine.cycle())) { }
            // Since there is no frame with a return address, the return statement of the function
            // we execute will fail to pop.
            if (cycle_result.error() != Execution_Error_Code::pop_call) {
                return Analysis_Error { cycle_result.error(), node.token };
            }
            BIT_MANIPULATION_ASSERT(constant_evaluation_machine.stack_size() == 1);
            node.const_value = constant_evaluation_machine.pop();
        }
        else {
            node.const_value = Value::unknown_of_type(return_type);
        }

        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Id_Expression_Node& node, Analysis_Level level, Expression_Context context)
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
        if (const auto* const looked_up_var = std::get_if<Let_Node>(&looked_up_node)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error { Analysis_Error_Code::let_variable_in_constant_expression,
                                        node.token, looked_up_var->token };
            }
            // TODO: in conjunction with potential changes to name lookup analysis, this would be a
            // god point to emit a "used before defined" diagnostic
            if (looked_up_var->const_value) {
                node.const_value = looked_up_var->const_value;
                return {};
            }
        }
        if (const auto* const looked_up_const = std::get_if<Const_Node>(&looked_up_node)) {
            // TODO: immediately analyze the const node?
            if (looked_up_const->const_value) {
                node.const_value = looked_up_const->const_value;
                return {};
            }
        }
        if (const auto* const looked_up_param = std::get_if<Parameter_Node>(&looked_up_node)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error { Analysis_Error_Code::parameter_in_constant_expression,
                                        node.token, looked_up_param->token };
            }
        }

        // TODO: is this actually necessary?
        if (auto r = analyze_types(node.lookup_result, level, context); !r) {
            return r;
        }
        const auto lookup_value = get_const_value(looked_up_node);
        BIT_MANIPULATION_ASSERT(lookup_value.has_value());
        if (context == Expression_Context::constant && lookup_value->is_unknown()) {
            return Analysis_Error { Analysis_Error_Code::expected_constant_expression, node.token,
                                    get_token(looked_up_node) };
        }
        node.const_value = lookup_value;
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_types(Literal_Node& node, Analysis_Level, Expression_Context)
    {
        // Literals never need to be analyzed twice.
        if (node.const_value) {
            BIT_MANIPULATION_ASSERT(node.const_value->int_value);
            return {};
        }
        switch (node.token.type) {
        case Token_Type::keyword_true: {
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
            node.const_value = Value::Int(*value);
            return {};
        }
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Given token type does not form a valid literal");
        }
    }
};

} // namespace

Result<void, Analysis_Error> analyze_semantics(Parsed_Program& program)
{
    Type_Analyzer analyzer { program };
    return analyzer();
}

} // namespace bit_manipulation::bms