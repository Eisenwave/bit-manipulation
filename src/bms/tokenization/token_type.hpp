#ifndef BIT_MANIPULATION_TOKEN_TYPE_HPP
#define BIT_MANIPULATION_TOKEN_TYPE_HPP

#include <compare>
#include <string_view>

#include "common/config.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Token_Type : Default_Underlying {
    // End of file token.
    // This token type is symbolic and doesn't match any part of the language itself,
    // but can be used to identify when the end of the file has been reached.
    eof,
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
    // "awoo"
    string_literal,
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
    // @
    at,
    // ;
    semicolon,
    // as
    keyword_as,
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
    // Nothing
    keyword_nothing,
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

/// @brief Returns `true` if the given `type` is a keyword.
[[nodiscard]] inline bool is_keyword(Token_Type type)
{
    return Default_Underlying(type) >= Default_Underlying(Token_Type::keyword_as);
}

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
/// Returns an empty string for variable-length tokens or `eof`,
/// which cannot be directly represented.
/// @param type the token type
/// @return The in-code name.
[[nodiscard]] std::string_view token_type_code_name(Token_Type type);

/// @brief Equivalent to `token_type_code_name(type).length()`.
[[nodiscard]] Size token_type_length(Token_Type type);

/// @brief Returns the corresponding `Type_Type`.
/// For example, `keyword_void` corresponds to `Type_Type::Void`.
[[nodiscard]] Type_Type token_type_type_type(Token_Type type);

[[nodiscard]] bool is_comment(Token_Type type);

[[nodiscard]] bool is_unary_operator(Token_Type type);

[[nodiscard]] bool is_arithmetic_operator(Token_Type type);

[[nodiscard]] bool is_bitwise_operator(Token_Type type);

[[nodiscard]] bool is_comparison_operator(Token_Type type);

[[nodiscard]] bool is_logical_operator(Token_Type type);

[[nodiscard]] bool is_relational_comparison_operator(Token_Type type);

[[nodiscard]] inline bool is_boolean_literal(Token_Type type)
{
    return type == Token_Type::keyword_true || type == Token_Type::keyword_false;
}

[[nodiscard]] bool is_integer_literal(Token_Type type);

[[nodiscard]] bool is_literal(Token_Type type);

[[nodiscard]] bool is_binary_operator(Token_Type type);

} // namespace bit_manipulation::bms

#endif
