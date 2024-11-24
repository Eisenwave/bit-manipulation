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

constexpr std::string_view evaluation_error_code_name(Evaluation_Error_Code code)
{
    switch (code) {
        using enum Evaluation_Error_Code;
        BIT_MANIPULATION_ENUM_STRING_CASE(type_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(conversion_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(division_by_zero);
        BIT_MANIPULATION_ENUM_STRING_CASE(shift_too_much);
        BIT_MANIPULATION_ENUM_STRING_CASE(assertion_fail);
    };
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

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

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code> check_unary_operator(Token_Type op,
                                                                              Concrete_Type value);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Type> args);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
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
