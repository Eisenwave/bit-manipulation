#include <iostream>
#include <unordered_map>

#include "common/parse.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/diagnostic_consumer.hpp"
#include "bms/evaluation/operations.hpp"
#include "bms/instantiate.hpp"
#include "bms/vm/codegen.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/vm.hpp"

namespace bit_manipulation::bms {

namespace {

constexpr bool debug_dump_generated_programs = false;

[[nodiscard]] Concrete_Value concrete_value_of(const ast::Some_Node& node)
{
    return get_const_value(node).value().concrete_value();
}

[[nodiscard]] Comparison_Failure comparison_failure_of(const ast::Binary_Expression& expression)
{
    const Concrete_Value l = concrete_value_of(*expression.get_left_node());
    const Concrete_Value r = concrete_value_of(*expression.get_right_node());
    return { l, r, expression.get_expression_type() };
}

enum struct Expression_Context : Default_Underlying {
    // The default context. Full analysis of functions and everything inside.
    normal,
    // A constant expression. Constant evaluation cannot run into an unknown value.
    constant,
};

struct Type_Analyzer {
private:
    Analyzed_Program& m_program;
    std::pmr::unsynchronized_pool_resource m_memory_resource;
    std::vector<ast::Function*> m_function_stack;

    struct Function_Stack_Scope {
        std::vector<ast::Function*>& m_function_stack;
        ~Function_Stack_Scope()
        {
            m_function_stack.pop_back();
        }
    };

    [[nodiscard]] Function_Stack_Scope push_function(ast::Function* info)
    {
        m_function_stack.push_back(info);
        return { m_function_stack };
    }

public:
    Type_Analyzer(Analyzed_Program& program, std::pmr::memory_resource* memory)
        : m_program(program)
        , m_memory_resource(memory)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        return analyze_types(m_program.get_root(), Analysis_Level::full,
                             Expression_Context::normal);
    }

private:
    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, Analysis_Level level, Expression_Context context)
    {
        if (handle == nullptr) {
            return {};
        }
        return visit(
            [this, handle, level, context](auto& node) {
                return analyze_types(handle, node, level, context); //
            },
            *handle);
    }

    [[gnu::always_inline]] Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                                                      ast::Program& node,
                                                                      Analysis_Level level,
                                                                      Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Program>(*handle) == &node);
        for (ast::Some_Node* h : node.get_children()) {
            auto r = analyze_types(h, level, Expression_Context::normal);
            if (!r) {
                return r;
            }
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Function& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Function>(*handle) == &node);
        BIT_MANIPULATION_ASSERT(level != Analysis_Level::unanalyzed);
        if (node.analysis_so_far >= level) {
            return {};
        }

        // If the function is generic, we can never say that analysis succeeded entirely because
        // new implicit instantiations may have been generated in the meantime.
        // For concrete functions, we can memoize the result of type analysis.
        if (node.is_generic) {
            for (const ast::Function::Instance& instance : node.instances) {
                auto r = analyze_types(instance.handle, level, Expression_Context::normal);
                if (!r) {
                    return r;
                }
            }
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.instances.empty());

        for (Parameter& parameter : node.get_parameters()) {
            auto r = analyze_types(parameter.get_type_node(), parameter.get_type(), level,
                                   Expression_Context::normal);
            if (!r) {
                return r;
            }
        }
        if (auto r = analyze_types(node.get_return_type_node(), node.get_return_type(), level,
                                   Expression_Context::normal);
            !r) {
            return r;
        }
        auto scope = push_function(&node);

        if (node.get_requires_clause_node() != nullptr) {
            auto& expr_const_value = get_const_value(*node.get_requires_clause_node());
            // Expressions in requires-clauses don't need to be checked twice, but we have to
            // determine this here instead of in the expression because expressions lack a
            // checking mechanism for themselves.
            if (!expr_const_value) {
                if (auto r = analyze_types(node.get_requires_clause_node(),
                                           Analysis_Level::for_constant_evaluation,
                                           Expression_Context::constant);
                    !r) {
                    return r;
                }
                node.const_value() = expr_const_value;
                BIT_MANIPULATION_ASSERT(node.const_value() && node.const_value()->is_known());

                if (node.const_value()->get_type() != Concrete_Type::Bool) {
                    return Analysis_Error_Builder { Analysis_Error_Code::requires_clause_not_bool }
                        .fail(node.get_requires_clause_node())
                        .cause(handle)
                        .build();
                }
                if (!node.const_value()->as_bool()) {
                    return Analysis_Error_Builder {
                        Analysis_Error_Code::requires_clause_not_satisfied
                    }
                        .fail(node.get_requires_clause_node())
                        .cause(handle)
                        .build();
                }
            }
        }

        if (node.analysis_so_far < Analysis_Level::shallow) {
            node.analysis_so_far = Analysis_Level::shallow;
        }
        if (level >= Analysis_Level::full) {
            if (auto r = analyze_types(node.get_body_node(), level, Expression_Context::normal);
                !r) {
                return r;
            }
        }
        if (level >= Analysis_Level::for_constant_evaluation) {
            auto& constant_evaluation_machine = m_program.get_vm();
            const Size vm_address = constant_evaluation_machine.instruction_count();
            Result<void, Analysis_Error> instructions
                = generate_code(constant_evaluation_machine.instructions(), handle, node);
            if (!instructions) {
                return instructions.error();
            }
            node.vm_address = vm_address;
            if constexpr (debug_dump_generated_programs) {
                dump_program(std::cout, constant_evaluation_machine.instructions());
            }
        }
        BIT_MANIPULATION_ASSERT(!node.is_generic);
        node.const_value() = get_const_value(*node.get_return_type_node());
        node.analysis_so_far = level;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, ast::Type& node, Analysis_Level, Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Type>(*handle) == &node);
        if (node.const_value()) {
            return {};
        }
        if (node.get_width_node() == nullptr) {
            node.const_value() = Value::unknown_of_type(node.concrete_type().value());
            return {};
        }

        if (auto r = analyze_types(node.get_width_node(), Analysis_Level::for_constant_evaluation,
                                   Expression_Context::constant);
            !r) {
            return r;
        }
        auto width = get_const_value(*node.get_width_node());
        BIT_MANIPULATION_ASSERT(width.has_value());
        if (!width->get_type().is_integer()) {
            return Analysis_Error_Builder { Analysis_Error_Code::width_not_integer }
                .fail(node.get_width_node())
                .build();
        }
        const Big_Int folded_width = width->as_int();
        if (folded_width <= 0 || folded_width > uint_max_width) {
            return Analysis_Error_Builder { Analysis_Error_Code::width_invalid }
                .fail(node.get_width_node())
                .value(Concrete_Value::Int(folded_width))
                .build();
        }
        node.concrete_width = static_cast<int>(folded_width);
        node.const_value() = Value::unknown_of_type(node.concrete_type().value());
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Const& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Const>(*handle) == &node);
        // Const nodes contain constant expressions, so if they have been analyzed in the past,
        // we can be sure that no further analysis is required.
        if (node.const_value()) {
            return {};
        }
        // Const nodes must always have an initializer.
        BIT_MANIPULATION_ASSERT(node.get_initializer_node() != nullptr);

        if (node.get_type_node() == nullptr) {
            if (auto r = analyze_types(node.get_initializer_node(),
                                       Analysis_Level::for_constant_evaluation,
                                       Expression_Context::constant);
                !r) {
                return r;
            }
            node.const_value() = get_const_value(*node.get_initializer_node());
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }

        ast::Type& type_node = node.get_type();
        const auto type_result
            = analyze_types(node.get_type_node(), type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }

        if (const auto r
            = analyze_types(node.get_initializer_node(), Analysis_Level::for_constant_evaluation,
                            Expression_Context::constant);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(*node.get_initializer_node());

        BIT_MANIPULATION_ASSERT(type_node.const_value().has_value());
        if (!initializer_value->get_type().is_convertible_to(type_node.const_value()->get_type())) {
            return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                .fail(handle)
                .cause(node.get_initializer_node())
                .build();
        }
        const Result<Value, Evaluation_Error_Code> r
            = evaluate_conversion(*initializer_value, type_node.const_value()->get_type());
        if (!r) {
            return Analysis_Error_Builder { r.error() }
                .fail(handle)
                .cause(node.get_initializer_node())
                .build();
        }
        node.const_value() = *r;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, ast::Let& node, Analysis_Level level, Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Let>(*handle) == &node);
        if (node.get_type_node() == nullptr) {
            // If there is no type, there must be an initializer.
            // This is "static type inference".
            // Prior analysis should have ensured this already, but we may a well double-check.
            BIT_MANIPULATION_ASSERT(node.get_initializer_node() != nullptr);

            if (auto r
                = analyze_types(node.get_initializer_node(), level, Expression_Context::normal);
                !r) {
                return r;
            }
            auto initializer_value = get_const_value(*node.get_initializer_node());
            BIT_MANIPULATION_ASSERT(initializer_value.has_value());
            // Intentionally "forget" the value determined during constant folding.
            // For mutable variables, it is possible that the value is modified, and
            // `const_value` should only be a concrete value for invariants.
            node.const_value() = Value::unknown_of_type(initializer_value->get_type());
            return {};
        }

        ast::Type& type_node = node.get_type();
        const auto type_result
            = analyze_types(node.get_type_node(), type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }
        if (node.get_initializer_node() == nullptr) {
            node.const_value() = Value::unknown_of_type(type_node.concrete_type().value());
            return {};
        }

        if (const auto r
            = analyze_types(node.get_initializer_node(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(*node.get_initializer_node());

        BIT_MANIPULATION_ASSERT(type_node.const_value().has_value());
        if (!initializer_value->get_type().is_convertible_to(type_node.const_value()->get_type())) {
            return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                .fail(handle)
                .cause(node.get_initializer_node())
                .build();
        }
        node.const_value() = Value::unknown_of_type(type_node.concrete_type().value());
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Static_Assert& node,
                                               Analysis_Level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Static_Assert>(*handle) == &node);
        // Static assertions never need to be checked twice.
        if (node.const_value()) {
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }
        auto expression_result
            = analyze_types(node.get_expression_node(), Analysis_Level::for_constant_evaluation,
                            Expression_Context::constant);
        if (!expression_result) {
            return expression_result;
        }
        node.const_value() = get_const_value(*node.get_expression_node());
        BIT_MANIPULATION_ASSERT(node.const_value() && node.const_value()->is_known());

        if (node.const_value()->get_type() != Concrete_Type::Bool) {
            return Analysis_Error_Builder { Analysis_Error_Code::static_assert_expression_not_bool }
                .fail(handle)
                .cause(node.get_expression_node())
                .build();
        }
        if (!node.const_value()->as_bool()) {
            Analysis_Error_Builder error { Analysis_Error_Code::static_assertion_failed };
            error.fail(handle).cause(node.get_expression_node());
            if (const auto* const comparison
                = get_if<ast::Binary_Expression>(node.get_expression_node());
                comparison && is_comparison_operator(comparison->get_op())) {
                error.comparison_failure(comparison_failure_of(*comparison));
            }
            return error.build();
        }

        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::If_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::If_Statement>(*handle) == &node);
        if (auto r = analyze_types(node.get_condition_node(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition_node());
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error_Builder { Analysis_Error_Code::condition_not_bool }
                .fail(handle)
                .cause(node.get_condition_node())
                .build();
        }

        auto if_result = analyze_types(node.get_if_block_node(), node.get_if_block(), //
                                       level, Expression_Context::normal);
        if (!if_result) {
            return if_result;
        }
        if (auto else_result
            = analyze_types(node.get_else_node(), level, Expression_Context::normal);
            !else_result) {
            return else_result;
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::While_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::While_Statement>(*handle) == &node);
        if (auto r = analyze_types(node.get_condition_node(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition_node());
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error_Builder { Analysis_Error_Code::condition_not_bool }
                .fail(handle)
                .cause(node.get_condition_node())
                .build();
        }

        if (auto r = analyze_types(node.get_block_node(), node.get_block(), level,
                                   Expression_Context::normal);
            !r) {
            return r;
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Control_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Control_Statement>(*handle) == &node);
        const auto* function = get_surrounding<ast::Function>(node);
        BIT_MANIPULATION_ASSERT(function);

        if (node.is_break() || node.is_continue()) {
            if (!get_surrounding<ast::While_Statement>(*handle)) {
                const auto error_code = node.is_break()
                    ? Analysis_Error_Code::break_outside_loop
                    : Analysis_Error_Code::continue_outside_loop;
                return Analysis_Error_Builder { error_code }.fail(handle).build();
            }
            node.const_value() = Value::Void;
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.is_return());

        const Concrete_Type return_type = function->get_return_type().concrete_type().value();

        if (!node.get_expression_node()) {
            if (return_type != Concrete_Type::Void) {
                return Analysis_Error_Builder {
                    Analysis_Error_Code::empty_return_in_non_void_function
                }
                    .fail(handle)
                    .cause(function->get_return_type_node())
                    .build();
            }
            node.const_value() = Concrete_Value::Void;
            return {};
        }

        if (auto r = analyze_types(node.get_expression_node(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto expr_value = get_const_value(*node.get_expression_node());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());
        if (!expr_value->get_type().is_convertible_to(return_type)) {
            return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                .fail(handle)
                .cause(function->get_return_type_node())
                .build();
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_conversion(*expr_value, return_type);
        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(return_type);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Assignment& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Assignment>(*handle) == &node);
        BIT_MANIPULATION_ASSERT(node.lookup_result);

        if (holds_alternative<Parameter*>(node.lookup_result)) {
            return Analysis_Error_Builder { Analysis_Error_Code::assigning_parameter }
                .fail(handle)
                .cause(*node.lookup_result)
                .build();
        }
        ast::Some_Node* looked_up_node = get<ast::Some_Node*>(node.lookup_result);
        if (holds_alternative<ast::Function>(*looked_up_node)) {
            return Analysis_Error_Builder { Analysis_Error_Code::assigning_function }
                .fail(handle)
                .cause(looked_up_node)
                .build();
        }

        if (holds_alternative<ast::Const>(*looked_up_node)) {
            return Analysis_Error_Builder { Analysis_Error_Code::assigning_const }
                .fail(handle)
                .cause(looked_up_node)
                .build();
        }

        auto& looked_up_var = get<ast::Let>(*looked_up_node);
        if (auto r = analyze_types(node.get_expression_node(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto expr_value = get_const_value(*node.get_expression_node());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Concrete_Type dest_type = looked_up_var.const_value().value().get_type();
        if (!expr_value->get_type().is_convertible_to(dest_type)) {
            return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                .fail(handle)
                .cause(node.get_expression_node())
                .build();
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_conversion(*expr_value, dest_type);
        if (!eval_result) {
            node.const_value() = Value::unknown_of_type(dest_type);
            return {};
        }
        node.const_value() = *eval_result;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Block_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Block_Statement>(*handle) == &node);
        for (ast::Some_Node* h : node.get_children()) {
            auto r = analyze_types(h, level, Expression_Context::normal);
            if (!r) {
                return r;
            }
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Conversion_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Conversion_Expression>(*handle) == &node);
        if (auto r = analyze_types(node.get_expression_node(), level, context); !r) {
            return r;
        }
        auto& target_type = node.get_target_type();
        if (auto r = analyze_types(node.get_target_type_node(), target_type, level, context); !r) {
            return r;
        }

        const auto& expression_value = get_const_value(*node.get_expression_node());
        BIT_MANIPULATION_ASSERT(expression_value);

        std::optional<Concrete_Type> type = target_type.concrete_type();
        BIT_MANIPULATION_ASSERT(type);

        if (!expression_value->get_type().is_convertible_to(*type)) {
            return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                .fail(handle)
                .cause(node.get_target_type_node())
                .build();
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_conversion(*expression_value, *type);

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::If_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::If_Expression>(*handle) == &node);
        if (auto r = analyze_types(node.get_condition_node(), level, context); !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition_node());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error_Builder { Analysis_Error_Code::condition_not_bool }
                .fail(handle)
                .cause(node.get_condition_node())
                .build();
        }
        Expression_Context left_context = context;
        Expression_Context right_context = context;
        // Similar to short-circuiting (see Binary_Expression), we lower the analysis context
        // to `normal` for the expression that is not evaluated.
        // If we analyzed it as a constant expression, we would require a value from it, and
        // e.g. `1 if true else 0 / 0` would fail, even though it is valid and meant to select `1`.
        if (context == Expression_Context::constant) {
            BIT_MANIPULATION_ASSERT(condition_value->is_known());
            const bool evaluate_left = condition_value->as_bool();
            (evaluate_left ? right_context : left_context) = Expression_Context::normal;
        }

        if (auto r = analyze_types(node.get_left_node(), level, left_context); !r) {
            return r;
        }
        auto left_value = get_const_value(*node.get_left_node());

        if (auto r = analyze_types(node.get_right_node(), level, right_context); !r) {
            return r;
        }
        auto right_value = get_const_value(*node.get_right_node());

        Result<Concrete_Type, Analysis_Error_Code> type_result = check_if_expression(
            left_value->get_type(), condition_value->get_type(), right_value->get_type());
        if (!type_result) {
            return Analysis_Error_Builder { type_result.error() }.fail(handle).build();
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_if_expression(*left_value, *condition_value, *right_value);

        // Type analysis has already succeeded, and for context expressions, the condition is known.
        // Therefore, it should be impossible for an evaluation to fail.
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant || eval_result.has_value());

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Binary_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Binary_Expression>(*handle) == &node);
        if (auto r = analyze_types(node.get_left_node(), level, context); !r) {
            return r;
        }
        const auto& left_value = get_const_value(*node.get_left_node());
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                || (left_value && left_value->is_known()));

        {
            // For short-circuiting operators like && and ||, if we are in a constant expression,
            // we should not evaluate the rest as a constant expression if short-circuiting takes
            // place.
            //
            // For example, in `const x = true || 0 / 0 == 0;`, the initializer is correct
            // according to type analysis, and the division by zero is never executed.
            // Therefore, only type analysis should be applied upon short-circuiting, not
            // constant expression analysis, which would also demand a value from `0 / 0`.
            const bool is_short_circuiting = node.get_op() == Token_Type::logical_and
                || node.get_op() == Token_Type::logical_or;
            if (context == Expression_Context::constant && is_short_circuiting) {
                if (left_value->get_type() != Concrete_Type::Bool) {
                    return Analysis_Error_Builder { Analysis_Error_Code::non_bool_logical }
                        .fail(handle)
                        .cause(node.get_left_node())
                        .build();
                }
                const bool circuit_breaker = node.get_op() == Token_Type::logical_or;
                if (left_value->as_bool() == circuit_breaker) {
                    context = Expression_Context::normal;
                }
            }
        }

        if (auto r = analyze_types(node.get_right_node(), level, context); !r) {
            return r;
        }
        const std::optional<Value>& right_value = get_const_value(*node.get_right_node());
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                || (right_value && right_value->is_known()));

        const Result<Concrete_Type, Analysis_Error_Code> type_result = check_binary_operator(
            left_value->get_type(), node.get_expression_type(), right_value->get_type());
        if (!type_result) {
            return Analysis_Error_Builder { type_result.error() }.fail(handle).build();
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_binary_operator(*left_value, node.get_expression_type(), *right_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error_Builder { eval_result.error() }.fail(handle).build();
        }

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Prefix_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Prefix_Expression>(*handle) == &node);
        if (auto r = analyze_types(node.get_expression_node(), level, context); !r) {
            return r;
        }
        const auto expr_value = get_const_value(*node.get_expression_node());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Result<Concrete_Type, Analysis_Error_Code> type_result
            = check_unary_operator(node.get_expression_type(), expr_value->get_type());
        if (!type_result) {
            return Analysis_Error_Builder { type_result.error() }.fail(handle).build();
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_unary_operator(node.get_expression_type(), *expr_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error_Builder { eval_result.error() }.fail(handle).build();
        }
        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Function_Call_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Function_Call_Expression>(*handle) == &node);
        BIT_MANIPULATION_ASSERT(node.lookup_result);

        // 1. Evaluate arguments.

        std::pmr::monotonic_buffer_resource temp_memory_resource;
        std::pmr::vector<Value> arg_values(&temp_memory_resource);
        arg_values.reserve(node.get_argument_count());

        for (ast::Some_Node* const arg : node.get_argument_nodes()) {
            if (auto r = analyze_types(arg, level, context); !r) {
                return r;
            }
            arg_values.push_back(get_const_value(*arg).value());
        }

        // 2.1. If the function is builtin, evaluate right away.

        if (Builtin_Function* builtin = get_if<Builtin_Function>(&node.lookup_result)) {
            auto type_result = check_builtin_function(*builtin, arg_values);
            if (!type_result) {
                return Analysis_Error_Builder { type_result.error() }
                    .fail(handle)
                    .cause(*builtin)
                    .build();
            }
            auto eval_result = evaluate_builtin_function(*builtin, arg_values);
            if (!eval_result) {
                return Analysis_Error_Builder { eval_result.error() }
                    .fail(handle)
                    .cause(*builtin)
                    .build();
            }
            BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                    || eval_result.has_value());
            node.const_value() = *eval_result;
            return {};
        }

        // 2.2. Otherwise, verify that we call a function.

        auto** looked_up_node = get_if<ast::Some_Node*>(&node.lookup_result);
        auto* function
            = looked_up_node == nullptr ? nullptr : get_if<ast::Function>(*looked_up_node);
        if (!function) {
            return Analysis_Error_Builder { Analysis_Error_Code::call_non_function }
                .fail(handle)
                .cause(*looked_up_node)
                .build();
        }

        // 3. Verify that we have the right number of arguments.
        //    We can perform this test prior to deduction because calling a function with the wrong
        //    number of arguments is an error before and after instantiating generic functions.
        //    There is no function overloading.

        std::span<const Parameter> possibly_generic_params = function->get_parameters();
        if (possibly_generic_params.size() != node.get_argument_count()) {
            return Analysis_Error_Builder { Analysis_Error_Code::wrong_number_of_arguments }
                .fail(handle)
                // TODO: use a cause location that is that of the parameter list, but that would
                // require storing extra source positions in ast::Function
                .cause(*looked_up_node)
                .build();
        }

        // 4. If necessary, perform bit-generic argument deduction.
        //    After deduction is complete, `function` will point to the concrete instance, not to
        //    the generic function anymore.
        if (function->is_generic) {
            // Functions with no parameters cannot be generic.
            BIT_MANIPULATION_ASSERT(!possibly_generic_params.empty());
            std::pmr::vector<int> deduced_widths(&temp_memory_resource);
            for (Size i = 0; i < node.get_argument_count(); ++i) {
                const ast::Type& type = possibly_generic_params[i].get_type();
                // If this function is generic, parameter types wouldn't have undergone analysis.
                BIT_MANIPULATION_ASSERT(!type.const_value());
                if (type.concrete_width) {
                    continue;
                }
                const auto* gen_expr = get_if<ast::Id_Expression>(type.get_width_node());
                if (!gen_expr || !gen_expr->bit_generic) {
                    continue;
                }
                if (!arg_values[i].get_type().is_uint()) {
                    return Analysis_Error_Builder {
                        Analysis_Error_Code::width_deduction_from_non_uint
                    }
                        .fail(node.get_argument_node(i))
                        .cause(type.get_width_node())
                        .build();
                }
                deduced_widths.push_back(arg_values[i].get_type().width());
            }

            Result<const ast::Function::Instance*, Analysis_Error> instantiation_result
                = instantiate_function(m_program, &m_memory_resource, *looked_up_node, *function,
                                       std::span<const int>(deduced_widths));
            if (!instantiation_result) {
                return instantiation_result.error();
            }

            // Using pointers is merely a workaround for Result not working with references.
            // The returned pointer is never null.
            BIT_MANIPULATION_ASSERT(*instantiation_result != nullptr);
            const auto& instance = **instantiation_result;
            function = &get<ast::Function>(*instance.handle);
            // Memoization of deduction results.
            // Future analysis of this function call expression will treat it as a call to a
            // concrete instance.
            node.lookup_result = instance.handle;
        }

        // The memoization in the first stage should ensure that we are now looking up the instance.
        // Alternatively, the function wasn't generic in the first place.
        BIT_MANIPULATION_ASSERT(!function->is_generic);

        // 5. Analyze the called concrete function.
        //    If we are in a constant expression, the whole function has to be immediately analyzed
        //    as a constant expression.
        //    Otherwise, we don't need to analyze the function body immediately.

        const auto inner_level = context == Expression_Context::constant
                || level == Analysis_Level::for_constant_evaluation
            ? Analysis_Level::for_constant_evaluation
            : Analysis_Level::shallow;

        // 5.1. It is possible that while analyzing a constant-evaluated function call,
        //      the surrounding function (directly or indirectly) would have to be analyzed.
        //      This is impossible, and we need to detect this case.

        // TODO: instead of a stack, we could also use a bool for functions and constants
        //       or simply all nodes, and track whether they are under analysis
        if (inner_level == Analysis_Level::for_constant_evaluation) {
            for (const ast::Function* info : m_function_stack) {
                if (function == info) {
                    // TODO: this should probably use a different diagnostic or the diagnostic
                    //       should be renamed
                    return Analysis_Error_Builder {
                        Analysis_Error_Code::codegen_call_to_unanalyzed
                    }
                        .fail(handle)
                        .cause(*looked_up_node)
                        .build();
                }
            }
        }

        auto r = analyze_types(*looked_up_node, *function, inner_level, context);
        if (!r) {
            return r;
        }

        BIT_MANIPULATION_ASSERT(function->analysis_so_far >= inner_level);
        Concrete_Type return_type = function->const_value()->get_type();

        // 6. Check whether the function can be called with the given arguments and obtain
        //    concrete values for the parameters if need be.

        const std::span<const Parameter> params = function->get_parameters();

        // 6.1. Make sure that function calls during constant evaluation have been compiled to VM.

        auto& constant_evaluation_machine = m_program.get_vm();
        if (context == Expression_Context::constant) {
            BIT_MANIPULATION_ASSERT(function->vm_address != ast::Function::invalid_vm_address);
            constant_evaluation_machine.reset();
            constant_evaluation_machine.jump_to(function->vm_address);
        }

        for (Size i = 0; i < node.get_argument_count(); ++i) {
            const Concrete_Type param_type = params[i].get_type().concrete_type().value();
            if (!arg_values[i].get_type().is_convertible_to(param_type)) {
                return Analysis_Error_Builder { Analysis_Error_Code::incompatible_types }
                    .fail(handle)
                    .cause(params[i].get_debug_info())
                    .build();
            }

            Result<Value, Evaluation_Error_Code> conv_result
                = evaluate_conversion(arg_values[i], param_type);
            if (context == Expression_Context::constant) {
                if (!conv_result) {
                    return Analysis_Error_Builder { conv_result.error() }
                        .fail(handle)
                        .cause(params[i].get_debug_info())
                        .build();
                }
                constant_evaluation_machine.push(conv_result->concrete_value());
            }
        }

        // 7. Constant-evaluate the function call.

        if (context == Expression_Context::constant) {
            Result<void, Execution_Error> cycle_result;
            while ((cycle_result = constant_evaluation_machine.cycle())) { }
            // Normally, the VM will push and pop function return addresses as frames
            // as part of execution function calls and returns.
            // However, we artificially jump directly to the start of the function without doing so,
            // meaning that there is no return address and we run into an execution error.
            // TODO: this design is questionable; maybe push a magic return address instead
            if (cycle_result.error().code != Execution_Error_Code::pop_call) {
                return Analysis_Error_Builder { cycle_result.error() }
                    .fail(handle)
                    .cause(cycle_result.error().debug_info)
                    .build();
            }
            BIT_MANIPULATION_ASSERT(constant_evaluation_machine.stack_size() == 1);
            node.const_value() = constant_evaluation_machine.pop();
        }
        else {
            node.const_value() = Value::unknown_of_type(return_type);
        }

        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Id_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Id_Expression>(*handle) == &node);
        if (node.const_value()) {
            // In instantiations of bit-generic id-expressions,
            // id-expressions don't turn into literals, but "magically" obtain a const_value.
            // In any other case, a lookup result should exist.
            // Id-expressions never need to be analyzed twice.
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.lookup_result);

        if (auto** parameter = get_if<Parameter*>(&node.lookup_result)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error_Builder {
                    Analysis_Error_Code::parameter_in_constant_expression
                }
                    .fail(handle)
                    .cause(node.lookup_result.value())
                    .build();
            }

            node.const_value() = (*parameter)->get_type().const_value();
            return {};
        }
        auto looked_up_node = get<ast::Some_Node*>(node.lookup_result);
        return analyze_types_for_id_looking_up_node(handle, node, looked_up_node, level, context);
    }

    Result<void, Analysis_Error>
    analyze_types_for_id_looking_up_node(ast::Some_Node* handle,
                                         ast::Id_Expression& node,
                                         ast::Some_Node* looked_up_node,
                                         Analysis_Level level,
                                         Expression_Context context)
    {
        if (const auto* const looked_up_function = get_if<ast::Function>(looked_up_node)) {
            return Analysis_Error_Builder { Analysis_Error_Code::function_in_expression }
                .fail(handle)
                .cause(looked_up_node)
                .build();
        }
        if (const auto* const looked_up_var = get_if<ast::Let>(looked_up_node)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error_Builder {
                    Analysis_Error_Code::let_variable_in_constant_expression
                }
                    .fail(handle)
                    .cause(looked_up_node)
                    .build();
            }
            if (!looked_up_var->const_value()) {
                return Analysis_Error_Builder { Analysis_Error_Code::use_of_undefined_variable }
                    .fail(handle)
                    .cause(looked_up_node)
                    .build();
            }
            node.const_value() = looked_up_var->const_value();
            return {};
        }
        if (const auto* const looked_up_const = get_if<ast::Const>(looked_up_node)) {
            // TODO: consider analyzing it from here, or analyzing global constants
            //       in a separate pass.
            if (!looked_up_const->const_value()) {
                return Analysis_Error_Builder { Analysis_Error_Code::use_of_undefined_constant }
                    .fail(handle)
                    .cause(looked_up_node)
                    .build();
            }
            node.const_value() = looked_up_const->const_value();
            return {};
        }

        // TODO: is this actually necessary?
        if (auto r = analyze_types(looked_up_node, level, context); !r) {
            return r;
        }
        const auto lookup_value = get_const_value(*looked_up_node);
        BIT_MANIPULATION_ASSERT(lookup_value.has_value());
        if (context == Expression_Context::constant && lookup_value->is_unknown()) {
            return Analysis_Error_Builder { Analysis_Error_Code::expected_constant_expression }
                .fail(handle)
                .cause(looked_up_node)
                .build();
        }
        node.const_value() = lookup_value;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, ast::Literal& node, Analysis_Level, Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(&get<ast::Literal>(*handle) == &node);
        // Literals never need to be analyzed twice.
        if (node.const_value()) {
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }
        switch (node.get_type()) {
        case Token_Type::keyword_true: {
            node.const_value() = Value::True;
            return {};
        }
        case Token_Type::keyword_false: {
            node.const_value() = Value::False;
            return {};
        }
        case Token_Type::binary_literal:
        case Token_Type::octal_literal:
        case Token_Type::decimal_literal:
        case Token_Type::hexadecimal_literal: {
            std::optional<Big_Int> value = parse_integer_literal(node.get_literal());
            if (!value) {
                return Analysis_Error_Builder { Analysis_Error_Code::invalid_integer_literal }
                    .fail(handle)
                    .build();
            }
            node.const_value() = Value::Int(*value);
            return {};
        }
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Given token type does not form a valid literal");
        }
    }
};

} // namespace

Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program,
                                               std::pmr::memory_resource* memory)
{
    Type_Analyzer analyzer { program, memory };
    return analyzer();
}

Result<void, Analysis_Error>
analyze(Analyzed_Program& program, const Parsed_Program& parsed, std::pmr::memory_resource* memory)
{
    std::pmr::unsynchronized_pool_resource local_memory(memory);
    if (auto r = resolve_annotations(program, parsed); !r) {
        return r;
    }
    if (auto r = analyze_name_lookup(program, &local_memory); !r) {
        return r;
    }
    local_memory.release();
    return analyze_semantics(program, &local_memory);
}

bool analyze(Analyzed_Program& program,
             const Parsed_Program& parsed,
             std::pmr::memory_resource* memory_resource,
             Diagnostic_Consumer& diagnostics)
{
    if (auto result = analyze(program, parsed, memory_resource)) {
        return true;
    }
    else {
        diagnostics(std::move(result.error()));
        return false;
    }
}

} // namespace bit_manipulation::bms
