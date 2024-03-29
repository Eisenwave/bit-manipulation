#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include <span>

#include "common/result.hpp"

#include "bms/fwd.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error_Code : Default_Underlying {
    /// @brief Type-checking of the evaluation failed.
    type_error,
    /// @brief Error during a conversion necessary for the evaluation.
    conversion_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
    /// @brief An assertion failed.
    assertion_fail,
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
    /// @brief Wrong number of arguments.
    wrong_number_of_arguments,
    /// @brief Wrong argument type for builtin function call.
    wrong_argument_type,
};

enum struct Builtin_Function : Default_Underlying {
    // `fn assert(cond: Bool) -> Void`
    assert
};

[[nodiscard]] constexpr Size builtin_parameter_count(Builtin_Function f)
{
    using enum Builtin_Function;
    switch (f) {
    case assert: return 1;
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function");
    }
}

// Type-only evaluations.

/// @brief Returns the common type between two types, or `std::nullopt` if there is none.
/// @param lhs the left type
/// @param rhs the right ype
/// @return The common type or `std::nullopt`.
[[nodiscard]] std::optional<Concrete_Type> get_common_type(Concrete_Type lhs, Concrete_Type rhs);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code> check_unary_operator(Token_Type op,
                                                                          Concrete_Type value);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Type> args);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args);

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Value> args);

// Concrete evaluations.

[[nodiscard]] Result<Concrete_Value, Conversion_Error_Code>
evaluate_conversion(Concrete_Value value, Concrete_Type to);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_unary_operator(Token_Type op, Concrete_Value value);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_binary_operator(Concrete_Value lhs, Token_Type op, Concrete_Value rhs);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_if_expression(Concrete_Value lhs, Concrete_Value condition, Concrete_Value rhs);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args);

// Evaluations.

[[nodiscard]] Result<Value, Conversion_Error_Code> evaluate_conversion(Value value,
                                                                       Concrete_Type to);

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_unary_operator(Token_Type op,
                                                                           Value value);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_if_expression(Value lhs, Value condition, Value rhs);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Value> args);

} // namespace bit_manipulation::bms

#endif