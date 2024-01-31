#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include "bms_tokens.hpp"
#include "bms_value.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error {
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
    /// @brief A binary or n-ary operation with incompatible types was attempted.
    incompatible_types,
    /// @brief A binary or n-ary operation with UInts of different widths was attempted.
    incompatible_widths,
    /// @brief When converting Int to Uint(N), the Int couldn't be represented.
    int_to_uint_range_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
};

struct Evaluation_Result {
private:
    std::variant<Value, Evaluation_Error> v;

public:
    [[nodiscard]] constexpr Evaluation_Result(Value value) noexcept
        : v(value)
    {
    }

    [[nodiscard]] constexpr Evaluation_Result(Evaluation_Error error) noexcept
        : v(error)
    {
    }

    [[nodiscard]] constexpr bool has_value() const
    {
        return v.index() == 0;
    }

    [[nodiscard]] constexpr Value get_value() const
    {
        return std::get<Value>(v);
    }

    [[nodiscard]] constexpr Evaluation_Error get_error() const
    {
        return std::get<Evaluation_Error>(v);
    }

    [[nodiscard]] constexpr explicit operator bool() const noexcept
    {
        return v.index() == 0;
    }

    [[nodiscard]] Value operator*() const
    {
        return std::get<Value>(v);
    }
};

[[nodiscard]] Evaluation_Result evaluate_unary_operator(Token_Type op, Value value) noexcept;

[[nodiscard]] Evaluation_Result
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept;

[[nodiscard]] Evaluation_Result
evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept;

} // namespace bit_manipulation::bms

#endif