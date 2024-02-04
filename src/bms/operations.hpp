#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include "result.hpp"

#include "bms/fwd.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error_Code : Default_Underlying {
    /// @brief Type-checking of the evaluation failed.
    type_error,
    /// @brief When converting Int to Uint(N), the Int couldn't be represented.
    int_to_uint_range_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
};

enum struct Type_Error_Code : Default_Underlying {
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
    /// @brief Arithmetic with bool was attempted, such as +true.
    int_bitwise,
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
    /// @brief The condition of an if expression is not a `Bool`.
    condition_not_bool,
};

// Type-only evaluations.

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_unary_operator(Token_Type op, Concrete_Type value) noexcept;

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs) noexcept;

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs) noexcept;

// Concrete evaluations.

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_conversion(Concrete_Value value, Concrete_Type to) noexcept;

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_unary_operator(Token_Type op, Concrete_Value value) noexcept;

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_binary_operator(Concrete_Value lhs, Token_Type op, Concrete_Value rhs) noexcept;

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_if_expression(Concrete_Value lhs, Concrete_Value condition, Concrete_Value rhs) noexcept;

// Evaluations.

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_conversion(Value value,
                                                                       Concrete_Type to) noexcept;

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_unary_operator(Token_Type op,
                                                                           Value value) noexcept;

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept;

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept;

} // namespace bit_manipulation::bms

#endif