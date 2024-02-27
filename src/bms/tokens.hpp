#ifndef BIT_MANIPULATION_BMS_TOKENS_HPP
#define BIT_MANIPULATION_BMS_TOKENS_HPP

#include <string_view>

#include "common/config.hpp"

#include "common/source_position.hpp"

namespace bit_manipulation::bms {

enum struct Token_Type : Default_Underlying {
    // identifier
    identifier,
    // (
    left_parenthesis,
    // )
    right_parenthesis,
    // 123
    decimal_literal,
    // 0123
    octal_literal,
    // 0xff
    hexadecimal_literal,
    // 0b1010
    binary_literal,
    // {
    left_brace,
    // }
    right_brace,
    // begin of C-style comment.
    block_comment,
    // C99, aka. double-slash comment.
    line_comment,
    // =
    assign,
    // ==
    equals,
    // !=
    not_equals,
    // +
    plus,
    // -
    minus,
    // *
    multiplication,
    // /
    division,
    // %
    remainder,
    // <
    less_than,
    // >
    greater_than,
    // <=
    less_or_equal,
    // >=
    greater_or_equal,
    // <<
    shift_left,
    // >>
    shift_right,
    // &
    bitwise_and,
    // |
    bitwise_or,
    // ~
    bitwise_not,
    // ^
    bitwise_xor,
    // &&
    logical_and,
    // ||
    logical_or,
    // !
    logical_not,
    // ->
    right_arrow,
    // =>
    double_right_arrow,
    // .
    dot,
    // :
    colon,
    // comma
    comma,
    // ;
    semicolon,
    // let
    keyword_let,
    // const
    keyword_const,
    // function
    keyword_function,
    // while
    keyword_while,
    // if
    keyword_if,
    // else
    keyword_else,
    // Uint
    keyword_uint,
    // Int
    keyword_int,
    // Bool,
    keyword_bool,
    // Void,
    keyword_void,
    // requires
    keyword_requires,
    // return
    keyword_return,
    // break
    keyword_break,
    // continue
    keyword_continue,
    // true
    keyword_true,
    // false
    keyword_false,
    // static_assert
    keyword_static_assert,
};

/// @brief Returns the enumeration name of a token type.
/// For example, `token_type_name(logical_not)` returns `"logical_not"`.
/// @param type the token type
/// @return The enumeration name.
[[nodiscard]] std::string_view token_type_name(Token_Type type);

/// @brief Returns a human-readable name of a token type.
/// For example, `token_type_name(logical_not)` returns `'!'`,
/// and `token_type_name(block_comment)` returns `"block comment"`.
/// In general, this will print any keywords and symbols in single-quotes, and other tokens as a
/// human-friendly name.
/// @param type the token type
/// @return The human-readable name.
[[nodiscard]] std::string_view token_type_readable_name(Token_Type type);

/// @brief Returns a human-readable name of a token type.
/// For example, `token_type_name(logical_not)` returns `!`.
/// Returns an empty string for variable-length tokens, which cannot be directly represented.
/// @param type the token type
/// @return The in-code name.
[[nodiscard]] std::string_view token_type_code_name(Token_Type type) noexcept;

[[nodiscard]] Size token_type_length(Token_Type type);

[[nodiscard]] bool is_comment(Token_Type type);

[[nodiscard]] bool is_unary_operator(Token_Type type);

[[nodiscard]] bool is_arithmetic_operator(Token_Type type);

[[nodiscard]] bool is_bitwise_operator(Token_Type type);

[[nodiscard]] bool is_comparison_operator(Token_Type type);

[[nodiscard]] bool is_logical_operator(Token_Type type);

[[nodiscard]] bool is_relational_comparison_operator(Token_Type type);

[[nodiscard]] bool is_literal(Token_Type type);

[[nodiscard]] bool is_binary_operator(Token_Type type);

/// @brief The operator precedence of a binary operator.
enum struct Binary_Operator_Precedence : Default_Underlying { comparison, arithmetic, none };

// We are kinda abusing `std::strong_ordering` here.
// BMS doesn't allow operator chaining, so there's no meaningful difference between unordered
// operators and operators with the same precedence.
// For example `x + y + z` is just as invalid as `x + y * z` so there's no difference between
// saying that `*` is unordered with `+`, and saying that they have the same precedence.

/// @brief Returns the ordering of two operator precedences.
/// `std::strong_ordering::less` represents that `x` has lower precedence than `y`.
/// For example, `+` has lower precedence than `*`.
/// @param x the left precedence
/// @param y the right precedence
/// @return `std::strong_ordering::equivalent` if the operators are unordered or have the same
/// precedence (there is no distinction in BMS), a `std::strong_ordering` which compares the
/// precedences otherwise.
constexpr std::strong_ordering operator<=>(Binary_Operator_Precedence x,
                                           Binary_Operator_Precedence y)
{
    return x == Binary_Operator_Precedence::none || y == Binary_Operator_Precedence::none
        ? std::strong_ordering::equivalent
        : static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
}

/// @brief Returns the next greater precedence, or `none` if none exists.
/// @param p the precedence
/// @return The next greater precedence, or `none` if none exists.
constexpr Binary_Operator_Precedence operator++(Binary_Operator_Precedence p)
{
    return Binary_Operator_Precedence { std::min<Default_Underlying>(
        static_cast<Default_Underlying>(p),
        static_cast<Default_Underlying>(Binary_Operator_Precedence::none)) };
}

[[nodiscard]] inline Binary_Operator_Precedence binary_operator_precedence_of(Token_Type x)
{
    return is_comparison_operator(x) ? Binary_Operator_Precedence::comparison
        : is_arithmetic_operator(x)  ? Binary_Operator_Precedence::arithmetic
                                     : Binary_Operator_Precedence::none;
}

struct Token {
    Local_Source_Span pos {};
    Token_Type type {};

    [[nodiscard]] Token() = default;

    [[nodiscard]] Token(const Local_Source_Span& pos, Token_Type type) noexcept
        : pos { pos }
        , type { type }
    {
    }

    friend constexpr auto operator<=>(Token, Token) = default;
};

} // namespace bit_manipulation::bms

#endif
