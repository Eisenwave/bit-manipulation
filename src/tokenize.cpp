#include <optional>

#include "bmscript.hpp"

namespace bit_manipulation {

namespace {

constexpr std::string_view identifier_characters = "abcdefghijklmnopqrstuvwxyz"
                                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                                   "0123456789_";

constexpr bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

std::optional<Token_Type> keyword_by_name(std::string_view s) noexcept
{
    using enum Token_Type;

    static constexpr struct {
        std::string_view name;
        Token_Type type;
    } types_by_name[] = {
        { "if", keyword_if },
        { "else", keyword_else },
        { "let", keyword_let },
        { "const", keyword_const },
        { "Int", keyword_int },
        { "Uint", keyword_uint },
        { "Bool", keyword_bool },
        { "break", keyword_break },
        { "continue", keyword_continue },
        { "return", keyword_return },
        { "function", keyword_function },
        { "true", keyword_true },
        { "false", keyword_false },
        { "requires", keyword_requires },
    };

    for (const auto [name, type] : types_by_name) {
        if (name == s) {
            return type;
        }
    }

    return std::nullopt;
}

std::optional<Token_Type> try_identify_fixed_length_token(std::string_view s) noexcept
{
    using enum Token_Type;

    if (s.empty()) {
        return std::nullopt;
    }
    switch (s[0]) {
    case '(': return left_parenthesis;
    case ')': return right_parenthesis;
    case '{': return left_brace;
    case '}': return right_brace;
    case '=': {
        if (s.length() > 1) {
            if (s[1] == '=') {
                return equals;
            }
            if (s[1] == '>') {
                return double_right_arrow;
            }
        }
        return assign;
    }

    case '+': return plus;
    case '-': {
        if (s.length() > 1 && s[1] == '>') {
            return right_arrow;
        }
        return minus;
    }
    case '*': {
        // comments are already handled elsewhere, so this is always multiplication
        return multiplication;
    }
    case '/':
        // comments are already handled elsewhere, so this is always division
        return division;
    case '%': return remainder;
    case '<': {
        if (s.length() > 1) {
            if (s[1] == '<') {
                return shift_left;
            }
            if (s[1] == '=') {
                return less_or_equal;
            }
            return less_than;
        }
    }
    case '>': {
        if (s.length() > 1) {
            if (s[1] == '>') {
                return shift_right;
            }
            if (s[1] == '=') {
                return greater_or_equal;
            }
        }
        return greater_than;
    }
    case '&': {
        if (s.length() > 1 && s[1] == '&') {
            return logical_and;
        }
        return bitwise_and;
    }
    case '|': {
        if (s.length() > 1 && s[1] == '|') {
            return logical_or;
        }
        return bitwise_or;
    }
    case '^': {
        return bitwise_xor;
    }
    case '~': {
        return bitwise_not;
    }
    case '!': {
        if (s.length() > 1 && s[1] == '=') {
            return not_equals;
        }
        return logical_not;
    }
    case ':': {
        return colon;
    }
    case ';': {
        return semicolon;
    }
    case '.': {
        return dot;
    }
    case ',': {
        return comma;
    }
    }
    return std::nullopt;
}

struct Tokenize_Result {
    Size length;
    Token_Type type;

    [[nodiscard]] explicit operator bool() const
    {
        return length != 0;
    }
};

Tokenize_Result try_tokenize_literal(std::string_view s)
{
    if (s.empty() || !is_digit(s[0])) {
        return {};
    }
    if (s[0] == '0') {
        if (s.length() > 1) {
            if (s[1] == 'x' || s[1] == 'b') {
                const Token_Type type
                    = s[1] == 'x' ? Token_Type::hexadecimal_literal : Token_Type::binary_literal;
                const std::string_view allowed_digits
                    = s[1] == 'x' ? "0123456789abcdefABCDEF" : "01";
                const Size digits = s.substr(2).find_first_not_of(allowed_digits);
                const Size result = digits == 0        ? 0
                    : digits == std::string_view::npos ? s.length()
                                                       : digits + 2;
                return { result, type };
            }
            const Size digits = s.find_first_not_of("01234567");
            return { std::min(digits, s.length()), Token_Type::octal_literal };
        }
        return { 1, Token_Type::decimal_literal };
    }
    const Size digits = s.find_first_not_of("0123456789");
    const Size result = digits == 0 ? 0 : digits == std::string_view::npos ? s.length() : digits;
    return { result, Token_Type::decimal_literal };
}

Size try_tokenize_identifier(std::string_view str)
{
    if (str.empty() || is_digit(str[0])) {
        return 0;
    }
    const Size result = str.find_first_not_of(identifier_characters);
    return std::min(result, str.length());
}

Tokenize_Result try_tokenize_identifier_or_keyword(std::string_view str)
{
    const Size identifier_length = try_tokenize_identifier(str);
    if (identifier_length == 0) {
        return { 0, Token_Type::identifier };
    }
    if (std::optional<Token_Type> keyword = keyword_by_name(str.substr(0, identifier_length))) {
        return { identifier_length, *keyword };
    }
    return { identifier_length, Token_Type::identifier };
}

std::optional<Tokenize_Result> try_tokenize(std::string_view s)
{
    if (s.starts_with("//")) {
        const Size length = std::min(s.find('\n', 2), s.length());
        return Tokenize_Result { length, Token_Type::line_comment };
    }
    if (s.starts_with("/*")) {
        // naive: nesting disallowed, but line comments can be nested in block comments
        const Size end = s.find("*/", 2);
        if (end != std::string_view::npos) {
            return Tokenize_Result { end + 2, Token_Type::block_comment };
        }
        // An unclosed comment should count as a tokenization failure.
        return std::nullopt;
    }
    if (const Tokenize_Result r = try_tokenize_identifier_or_keyword(s)) {
        return Tokenize_Result { r.length, r.type };
    }
    if (const Tokenize_Result r = try_tokenize_literal(s)) {
        return Tokenize_Result { r.length, r.type };
    }
    if (const std::optional<Token_Type> type = try_identify_fixed_length_token(s)) {
        return Tokenize_Result { token_type_length(*type), *type };
    }
    return std::nullopt;
}

Source_Position advance_position_by_text(Source_Position pos, std::string_view text)
{
    for (char c : text) {
        switch (c) {
        case '\t': pos.column += 4; break;
        case '\r': pos.column = 0; break;
        case '\n':
            pos.column = 0;
            pos.line += 1;
            break;
        default: pos.column += 1;
        }
        pos.begin += 1;
    }
    return pos;
}

} // namespace

Tokenize_Error tokenize(std::vector<Token>& out, std::string_view source) noexcept
{
    Source_Position pos {};

    while (true) {
        std::string_view remainder = source.substr(pos.begin);
        if (remainder.empty()) {
            break;
        }
        if (const Size whitespace_length = remainder.find_first_not_of(" \r\t\n")) {
            if (whitespace_length == std::string_view::npos) {
                break;
            }
            pos = advance_position_by_text(pos, remainder.substr(0, whitespace_length));
            remainder = remainder.substr(whitespace_length);
        }

        if (std::optional<Tokenize_Result> part = try_tokenize(remainder)) {
            out.push_back({ pos, part->length, part->type });
            pos.begin += part->length;
            pos.column += part->length;
        }
        else {
            return { Tokenize_Error_Code::illegal_character, pos };
        }
    }

    return {};
}

} // namespace bit_manipulation