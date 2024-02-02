#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error {
    /// @brief No error.
    ok,
    /// @brief Type-checking of the evaluation failed.
    type_error,
    /// @brief When converting Int to Uint(N), the Int couldn't be represented.
    int_to_uint_range_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
};

enum struct Type_Error_Code {
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
    /// @brief The condition of an if expression is not a `Bool`.
    condition_not_bool,
};

namespace detail {

template <typename T, typename Error>
struct Evaluation_Result_Impl {
private:
    std::variant<Value, Error> v;

public:
    [[nodiscard]] constexpr Evaluation_Result_Impl(T value) noexcept
        : v(value)
    {
    }

    [[nodiscard]] constexpr Evaluation_Result_Impl(Error error) noexcept
        : v(error)
    {
    }

    [[nodiscard]] constexpr bool has_value() const
    {
        return v.index() == 0;
    }

    [[nodiscard]] constexpr T get_value() const
    {
        return std::get<T>(v);
    }

    [[nodiscard]] constexpr Error get_error() const
    {
        return std::get<Error>(v);
    }

    [[nodiscard]] constexpr explicit operator bool() const noexcept
    {
        return v.index() == 0;
    }

    [[nodiscard]] T operator*() const
    {
        return std::get<T>(v);
    }
};

} // namespace detail

// Type-only evaluations.

using Type_Evaluation_Result = detail::Evaluation_Result_Impl<Concrete_Type, Type_Error_Code>;

[[nodiscard]] Type_Evaluation_Result check_unary_operator(Token_Type op,
                                                          Concrete_Type value) noexcept;

[[nodiscard]] Type_Evaluation_Result
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs) noexcept;

[[nodiscard]] Type_Evaluation_Result
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs) noexcept;

// Concrete evaluations.

using Concrete_Evaluation_Result = detail::Evaluation_Result_Impl<Concrete_Value, Evaluation_Error>;

[[nodiscard]] Concrete_Evaluation_Result evaluate_conversion(Concrete_Value value,
                                                             Concrete_Type to) noexcept;

[[nodiscard]] Concrete_Evaluation_Result evaluate_unary_operator(Token_Type op,
                                                                 Concrete_Value value) noexcept;

[[nodiscard]] Concrete_Evaluation_Result
evaluate_binary_operator(Concrete_Value lhs, Token_Type op, Concrete_Value rhs) noexcept;

[[nodiscard]] Concrete_Evaluation_Result
evaluate_if_expression(Concrete_Value lhs, Concrete_Value condition, Concrete_Value rhs) noexcept;

// Evaluations.

using Evaluation_Result = detail::Evaluation_Result_Impl<Value, Evaluation_Error>;

[[nodiscard]] Evaluation_Result evaluate_conversion(Value value, Concrete_Type to) noexcept;

[[nodiscard]] Evaluation_Result evaluate_unary_operator(Token_Type op, Value value) noexcept;

[[nodiscard]] Evaluation_Result
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept;

[[nodiscard]] Evaluation_Result
evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept;

} // namespace bit_manipulation::bms

#endif