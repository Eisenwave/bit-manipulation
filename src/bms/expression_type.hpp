#ifndef BIT_MANIPULATION_EXPRESSION_TYPE_HPP
#define BIT_MANIPULATION_EXPRESSION_TYPE_HPP

#include <compare>
#include <string_view>

#include "common/assert.hpp"
#include "common/config.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Expression_Type : Default_Underlying {
    if_expression,
    conversion,

    logical_and,
    logical_or,

    equals,
    not_equals,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,

    binary_plus,
    binary_minus,
    multiplication,
    division,
    remainder,

    shift_left,
    shift_right,
    bitwise_and,
    bitwise_or,
    bitwise_xor,

    unary_plus,
    unary_minus,
    logical_not,
    bitwise_not,

    function_call,

    literal,
    id,
};

[[nodiscard]] Token_Type expression_type_token(Expression_Type type);

[[nodiscard]] std::string_view expression_type_code_name(Expression_Type type);

[[nodiscard]] constexpr bool is_primary(Expression_Type type) noexcept
{
    return type == Expression_Type::literal || type == Expression_Type::id;
}

[[nodiscard]] constexpr bool is_postfix_unary(Expression_Type type) noexcept
{
    return type == Expression_Type::function_call;
}

[[nodiscard]] constexpr bool is_prefix_unary(Expression_Type type) noexcept
{
    using enum Expression_Type;
    switch (type) {
    case unary_plus:
    case unary_minus:
    case logical_not:
    case bitwise_not: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_equality_comparison(Expression_Type type) noexcept
{
    return type == Expression_Type::equals || type == Expression_Type::not_equals;
}

[[nodiscard]] constexpr bool is_relational_comparison(Expression_Type type) noexcept
{
    using enum Expression_Type;
    switch (type) {
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_binary_comparison(Expression_Type type) noexcept
{
    return is_equality_comparison(type) || is_relational_comparison(type);
}

[[nodiscard]] constexpr bool is_unary_arithmetic(Expression_Type type) noexcept
{
    return type == Expression_Type::unary_plus || type == Expression_Type::unary_minus;
}

[[nodiscard]] constexpr bool is_binary_arithmetic(Expression_Type type) noexcept
{
    using enum Expression_Type;
    switch (type) {
    case binary_plus:
    case binary_minus:
    case multiplication:
    case division:
    case remainder: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_arithmetic(Expression_Type type) noexcept
{
    return is_unary_arithmetic(type) || is_binary_arithmetic(type);
}

[[nodiscard]] constexpr bool is_binary_bitwise(Expression_Type type) noexcept
{
    using enum Expression_Type;
    switch (type) {
    case bitwise_and:
    case bitwise_or:
    case bitwise_xor:
    case shift_left:
    case shift_right: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_binary_logical(Expression_Type type) noexcept
{
    return type == Expression_Type::logical_and || type == Expression_Type::logical_or;
}

[[nodiscard]] constexpr bool is_bitwise(Expression_Type type) noexcept
{
    return type == Expression_Type::bitwise_not || is_binary_bitwise(type);
}

[[nodiscard]] constexpr bool is_binary(Expression_Type type) noexcept
{
    return Default_Underlying(type) >= Default_Underlying(Expression_Type::conversion)
        && Default_Underlying(type) <= Default_Underlying(Expression_Type::bitwise_xor);
}

namespace detail {

/// @brief A minimal group of expressions within which any expression would have the same
/// precedence.
enum struct Precedence_Group : Default_Underlying {
    conversion_and_if,
    binary_logical,
    binary_comparison,
    binary_arithmetic,
    binary_bitwise,
    prefix_unary,
    postfix_unary,
    primary
};

[[nodiscard]] constexpr bool is_binary(Precedence_Group group) noexcept
{
    using enum Precedence_Group;
    switch (group) {
    case binary_logical:
    case binary_comparison:
    case binary_arithmetic:
    case binary_bitwise: return true;
    default: return false;
    }
}

constexpr std::partial_ordering operator<=>(Precedence_Group x, Precedence_Group y) noexcept
{
    using enum Precedence_Group;
    if (x == y) {
        return std::partial_ordering::unordered;
    }
    if (is_binary(x) && is_binary(y)) {
        if (x == binary_comparison && y == binary_arithmetic) {
            return std::partial_ordering::less;
        }
        if (y == binary_comparison && x == binary_arithmetic) {
            return std::partial_ordering::greater;
        }
        return std::partial_ordering::unordered;
    }
    const std::strong_ordering result
        = static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
    return result == std::strong_ordering::equal ? std::partial_ordering::unordered : result;
}

[[nodiscard]] constexpr Precedence_Group precedence_group_of(Expression_Type type)
{
    using enum Expression_Type;
    switch (type) {
    case conversion:
    case if_expression: return Precedence_Group::conversion_and_if;

    case equals:
    case not_equals:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal: return Precedence_Group::binary_comparison;

    case binary_plus:
    case binary_minus:
    case multiplication:
    case division:
    case remainder: return Precedence_Group::binary_arithmetic;

    case shift_left:
    case shift_right:
    case bitwise_and:
    case bitwise_or:
    case bitwise_xor:
    case logical_and:
    case logical_or: return Precedence_Group::binary_bitwise;

    case unary_plus:
    case unary_minus:
    case logical_not:
    case bitwise_not: return Precedence_Group::prefix_unary;

    case function_call: return Precedence_Group::postfix_unary;

    case literal:
    case id: return Precedence_Group::primary;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid expression type.");
};

} // namespace detail

/// @brief Returns the precedence between two expressions `x` and `y`.
/// For example, `compare_precedence(equals, binary_plus)` is `less`, meaning that `equals` has
/// lower precedence and plus can be nested within equality comparison without parentheses
/// for disambiguation, i.e. `a == b + 3` is valid.
///
/// `greater` means that the precedence is the outer way around.
/// `equivalent` implies that the two operators have the same precedence, usually because the same
/// input was given for both parameters, and that operator chaining is allowed.
/// `unordered` means that there is no clear precedence and parentheses are required.
///
/// Since BMS has very strict parenthesization rules, most comparisons are simply `unordered`.
constexpr std::partial_ordering compare_precedence(Expression_Type x, Expression_Type y)
{
    return detail::precedence_group_of(x) <=> detail::precedence_group_of(y);
}

} // namespace bit_manipulation::bms

#endif
