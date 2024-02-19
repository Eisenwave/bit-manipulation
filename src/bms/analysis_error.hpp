#ifndef BIT_MANIPULATION_BMS_BMS_HPP
#define BIT_MANIPULATION_BMS_BMS_HPP

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"
#include "bms/operations.hpp"

namespace bit_manipulation::bms {

// TODO: this doesn't really belong here, or the header should be renamed
enum struct Analysis_Level : Default_Underlying {
    /// @brief No analysis has taken place.
    unanalyzed,
    /// @brief For analyzing functions only when called by other functions, i.e. it's not
    /// necessary to analyze the function body.
    shallow,
    /// @brief Full analysis of a function, including its body.
    full,
    /// @brief Full analysis, recursively, for analyzing functions in constant expressions.
    deep
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
    /// @brief The width of an integer is not a constant expression.
    width_not_const,
    /// @brief The width of an integer exceeds the maximum (`uint_max_width`).
    width_too_large,
    /// @brief The width of an integer is zero.
    width_zero,
    /// @brief Expected a constant expression, but was unable to perform constant folding.
    expected_constant_expression,
    /// @brief Attempted to use a variable not declared 'const' in a constant expression.
    let_variable_in_constant_expression,
    /// @brief Attempted to use a function parameter in a constant expression.
    parameter_in_constant_expression,
    /// @brief Attempted to use a function in an expression as if it was a variable.
    function_in_expression,
    /// @brief Type error in expressions, implicit conversions, etc.
    type_error,
    /// @brief Error in the execution of the generated code for constexpr functions.
    execution_error,
    /// @brief Evaluation error in constant expressions or constant folding.
    evaluation_error,
    /// @brief Error in attempted implicit or explicit conversion.
    conversion_error,
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
};

struct Comparison_Failure {
    Concrete_Value left, right;
    Token_Type op;
};

struct Analysis_Error {
private:
    Analysis_Error_Code m_code {};
    union {
        Type_Error_Code m_type_error;
        Evaluation_Error_Code m_evaluation_error;
        Execution_Error_Code m_execution_error;
        Conversion_Error_Code m_conversion_error;
    };

public:
    std::optional<Comparison_Failure> comparison_failure;
    const ast::Some_Node* fail = nullptr;
    const ast::Some_Node* cause = nullptr;

    [[nodiscard]] constexpr Analysis_Error(Analysis_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(code)
        , fail(fail)
        , cause(cause)
    {
    }

    [[nodiscard]] constexpr Analysis_Error(Comparison_Failure comp_fail,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(Analysis_Error_Code::static_assertion_failed)
        , comparison_failure(comp_fail)
        , fail(fail)
        , cause(cause)
    {
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

    [[nodiscard]] constexpr Analysis_Error(Type_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(Analysis_Error_Code::type_error)
        , m_type_error(code)
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

    [[nodiscard]] constexpr Analysis_Error(Conversion_Error_Code code,
                                           const ast::Some_Node* fail,
                                           const ast::Some_Node* cause = {})
        : m_code(Analysis_Error_Code::conversion_error)
        , m_conversion_error(code)
        , fail(fail)
        , cause(cause)
    {
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

    [[nodiscard]] constexpr Type_Error_Code type_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code == Analysis_Error_Code::type_error);
        return m_type_error;
    }

    [[nodiscard]] constexpr Execution_Error_Code execution_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code == Analysis_Error_Code::execution_error);
        return m_execution_error;
    }

    [[nodiscard]] constexpr Conversion_Error_Code conversion_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code == Analysis_Error_Code::conversion_error);
        return m_conversion_error;
    }
};

} // namespace bit_manipulation::bms

#endif