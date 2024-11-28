#ifndef BIT_MANIPULATION_BMS_BMS_HPP
#define BIT_MANIPULATION_BMS_BMS_HPP

#include <optional>
#include <string_view>

#include "bms/comparison_failure.hpp"
#include "bms/concrete_value.hpp"
#include "bms/execution_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

// TODO: this doesn't really belong here, or the header should be renamed

/// @brief The level of analysis to perform on an AST node that represents a definable entity,
/// such as a function.
/// @see ast::Function::analysis_so_far
enum struct Analysis_Level : Default_Underlying {
    /// @brief No analysis has taken place.
    unanalyzed,
    /// @brief Analysis only of the declaration, but not of the definition.
    /// For example, a function undergoes shallow analysis when it is called
    /// so that type checking (matching arguments to parameters) can be done.
    shallow,
    /// @brief Full analysis of a function, including its body.
    /// This is done when e.g. a function definition appears in code,
    /// and the contents of its body should be analyzed.
    full,
    /// @brief Full analysis, recursively, for analyzing functions called in constant expressions.
    /// In addition to the requirements posed by `full`,
    /// this requires a definition of any called function to be available, recursively.
    /// These requirements are extremely similar to requirements on `constexpr` functions.
    for_constant_evaluation
};

constexpr auto operator<=>(Analysis_Level x, Analysis_Level y) noexcept
{
    return static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
}

enum struct Analysis_Error_Code : Default_Underlying {
    /// @brief A name was already in use when attempting to define a global constant.
    failed_to_define_global_const,
    /// @brief A name was already in use when attempting to define a function.
    failed_to_define_function,
    /// @brief A name was already in use when attempting to define a function parameter.
    failed_to_define_parameter,
    /// @brief A name was already in use when attempting to define a variable.
    failed_to_define_variable,
    /// @brief An expression attempted to look up a variable or parameter, but it was not defined.
    reference_to_undefined_variable,
    /// @brief An assignment attempted to assign a variable which is not defined.
    assignment_of_undefined_variable,
    /// @brief A function call to an undefined function was attempted.
    call_to_undefined_function,
    /// @brief The width of an integer is not an integer. (e.g. Uint(true))
    width_not_integer,
    /// @brief The width of an integer exceeds the maximum (`uint_max_width`), is zero, or negative.
    width_invalid,
    /// @brief Expected a constant expression, but was unable to perform constant folding.
    expected_constant_expression,
    /// @brief Attempted to use a variable not declared 'const' in a constant expression.
    let_variable_in_constant_expression,
    /// @brief Attempted to use a function parameter in a constant expression.
    parameter_in_constant_expression,
    /// @brief Attempted to use a function in an expression as if it was a variable.
    function_in_expression,
    /// @brief Error in the execution of the generated code for constexpr functions.
    execution_error,
    /// @brief Evaluation error in constant expressions or constant folding.
    evaluation_error,
    /// @brief Condition of an if statement or while loop is not a `Bool`.
    condition_not_bool,
    /// @brief A given literal is invalid, possibly because it is too large for the compiler's
    /// internal integer representation.
    invalid_integer_literal,
    /// @brief An assignment attempted to assign a function parameter, but function parameters are
    // immutable.
    assigning_parameter,
    /// @brief An attempt was made to assign to a function.
    assigning_function,
    /// @brief An attempt was made to assign to a constant.
    assigning_const,
    /// @brief A function call attempted to call a symbol which is not a function.
    call_non_function,
    /// @brief A function call with the wrong number of arguments was attempted.
    wrong_number_of_arguments,
    /// @brief Constant evaluation depends on a function whose definition is not yet complete.
    codegen_call_to_unanalyzed,
    /// @brief Attempting to deduce the width of `UInt(N)` from a type that is not `Uint`.
    width_deduction_from_non_uint,
    /// @brief The expression in a static assertion is not of type `Bool`.
    static_assert_expression_not_bool,
    /// @brief The expression in a static assertion evaluated to `false`.
    static_assertion_failed,
    /// @brief The expression in a requires-clause is not of type `Bool`.
    requires_clause_not_bool,
    /// @brief The expression in a requires clause evaluated to `false`.
    requires_clause_not_satisfied,
    /// @brief Use of undefined variable.
    use_of_undefined_variable,
    /// @brief Use of undefined constant.
    use_of_undefined_constant,
    /// @brief Empty return statement in non-void function.
    empty_return_in_non_void_function,

    // FIXME: this diagnostic might be dead and should be an assertion instead
    /// @brief Use of an invalid operator.
    invalid_operator,

    /// @brief An operation involving Void was attempted.
    void_operation,
    /// @brief Arithmetic with bool was attempted, such as +true.
    bool_arithmetic,
    /// @brief A bitwise operation with bool was attempted, such as ~false.
    bool_bitwise,
    /// @brief a relational comparison with bool was attempted.
    bool_relational_comparison,
    /// @brief Bitwise operators were applied to Int.
    int_bitwise,

    // FIXME: int_logical and uint_logical may be dead because of non_bool_logical
    /// @brief Logical operators were applied to Int.
    int_logical,
    /// @brief Logical operators were applied to Uint.
    uint_logical,

    /// @brief Logical operators were applied to something other than bool.
    non_bool_logical,
    /// @brief A binary or n-ary operation with incompatible types was attempted.
    incompatible_types,
    /// @brief A binary or n-ary operation with UInts of different widths was attempted.
    incompatible_widths,
    /// @brief Wrong argument type for builtin function call.
    wrong_argument_type,
    /// @brief A break statement appeared outside of a loop.
    break_outside_loop,
    /// @brief A continue statement appeared outside of a loop.
    continue_outside_loop,
};

constexpr std::string_view analysis_error_code_name(Analysis_Error_Code code)
{
    switch (code) {
        using enum Analysis_Error_Code;
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_global_const);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(reference_to_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(assignment_of_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(call_to_undefined_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_not_integer);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_invalid);
        BIT_MANIPULATION_ENUM_STRING_CASE(expected_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(let_variable_in_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(parameter_in_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_in_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(execution_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(evaluation_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(condition_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(invalid_integer_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_const);
        BIT_MANIPULATION_ENUM_STRING_CASE(call_non_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(wrong_number_of_arguments);
        BIT_MANIPULATION_ENUM_STRING_CASE(codegen_call_to_unanalyzed);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_deduction_from_non_uint);
        BIT_MANIPULATION_ENUM_STRING_CASE(static_assert_expression_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(static_assertion_failed);
        BIT_MANIPULATION_ENUM_STRING_CASE(requires_clause_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(requires_clause_not_satisfied);
        BIT_MANIPULATION_ENUM_STRING_CASE(use_of_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(use_of_undefined_constant);
        BIT_MANIPULATION_ENUM_STRING_CASE(empty_return_in_non_void_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(invalid_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(void_operation);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_arithmetic);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_bitwise);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_relational_comparison);
        BIT_MANIPULATION_ENUM_STRING_CASE(int_bitwise);
        BIT_MANIPULATION_ENUM_STRING_CASE(int_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(uint_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(non_bool_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(incompatible_types);
        BIT_MANIPULATION_ENUM_STRING_CASE(incompatible_widths);
        BIT_MANIPULATION_ENUM_STRING_CASE(wrong_argument_type);
        BIT_MANIPULATION_ENUM_STRING_CASE(break_outside_loop);
        BIT_MANIPULATION_ENUM_STRING_CASE(continue_outside_loop);
    };
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

/// @brief A high-level error that occurred during program analysis.
/// No matter the cause (name lookup, type checking, execution errors, failed assertions, etc.),
/// everything turns into an `Analysis_Error` at some point.
struct Analysis_Error {
private:
    Analysis_Error_Code m_code {};
    union {
        Evaluation_Error_Code m_evaluation_error;
        Execution_Error_Code m_execution_error;
    };

public:
    /// @brief An optional comparison failure.
    /// This can be added onto errors such as failed `static_assert` or failed calls to `assert`
    /// during constant evaluation, in order to provide detail about the inputs.
    std::optional<Comparison_Failure> comparison_failure;
    /// @brief The node at which the failure took place.
    const ast::Some_Node* fail = nullptr;
    /// @brief The node which is considered to be a cause for the failure,
    /// but not the location of the failure.
    /// For example, if we attempt `x = 0` where `x` is `const`,
    /// the assignment node would be considered the `fail`,
    /// and the constant declaration of `x` would be considered the cause.
    const ast::Some_Node* cause = nullptr;

    /// @brief Constructs an error with an `Analysis_Error_Code`.
    /// This constructor should only be used for errors when none of the specialized constructors
    /// below apply.
    /// @param code the error code, which shall be none of `type_error`, `evaluation_error`,
    /// `execution_error`, or `conversion_error`
    /// @param fail the fail node
    /// @param cause the cause node
    [[nodiscard]] constexpr Analysis_Error(Analysis_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(code)
        , fail(fail)
        , cause(cause)
    {
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::evaluation_error);
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::execution_error);
    }

    [[nodiscard]] constexpr Analysis_Error(Evaluation_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(Analysis_Error_Code::evaluation_error)
        , m_evaluation_error(code)
        , fail(fail)
        , cause(cause)
    {
    }

    [[nodiscard]] constexpr Analysis_Error(Execution_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(Analysis_Error_Code::execution_error)
        , m_execution_error(code)
        , fail(fail)
        , cause(cause)
    {
    }

    /// @brief Constructs from an `Execution_Error`.
    /// This correctly treats execution errors that result from evaluation errors as
    /// evaluation errors in analysis.
    ///
    /// For example, if execution runs into `x / 0` (division by zero),
    /// the analysis error is then an `Evaluation_Error_Code::division_by_zero`,
    /// not just some generic `Analysis_Error_Code::execution_error`.
    [[nodiscard]] constexpr Analysis_Error(const Execution_Error& error,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : comparison_failure(error.comparison_failure)
        , fail(fail)
        , cause(cause)

    {
        if (error.code == Execution_Error_Code::evaluation) {
            m_code = Analysis_Error_Code::evaluation_error;
            m_evaluation_error = error.evaluation_error;
        }
        else {
            m_code = Analysis_Error_Code::execution_error;
        }
    }

    [[nodiscard]] constexpr Analysis_Error_Code code() const
    {
        return m_code;
    }

    [[nodiscard]] constexpr Evaluation_Error_Code evaluation_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code == Analysis_Error_Code::evaluation_error);
        return m_evaluation_error;
    }

    [[nodiscard]] constexpr Execution_Error_Code execution_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code == Analysis_Error_Code::execution_error);
        return m_execution_error;
    }
};

} // namespace bit_manipulation::bms

#endif
