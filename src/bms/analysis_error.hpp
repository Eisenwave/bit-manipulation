#ifndef BIT_MANIPULATION_BMS_BMS_HPP
#define BIT_MANIPULATION_BMS_BMS_HPP

#include "bms/operations.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

enum struct Analysis_Error_Code {
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
};

struct Analysis_Error {
    Analysis_Error_Code code {};
    Type_Error type_error {};
    Evaluation_Error evaluation_error {};
    Token fail_token {};
    Token cause_token {};

    constexpr Analysis_Error(Analysis_Error_Code code, Token fail_token, Token cause_token = {})
        : code(code)
        , evaluation_error()
        , fail_token(fail_token)
        , cause_token(cause_token)
    {
    }

    constexpr Analysis_Error(Evaluation_Error code, Token fail_token, Token cause_token = {})
        : code(Analysis_Error_Code::evaluation_error)
        , evaluation_error(code)
        , fail_token(fail_token)
        , cause_token(cause_token)
    {
    }

    constexpr Analysis_Error(Type_Error code, Token fail_token, Token cause_token = {})
        : code(Analysis_Error_Code::type_error)
        , type_error(code)
        , fail_token(fail_token)
        , cause_token(cause_token)
    {
    }
};

} // namespace bit_manipulation::bms

#endif