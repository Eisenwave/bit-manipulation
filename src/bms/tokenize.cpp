#include <optional>

#include "assert.hpp"

#include "common/parse.hpp"

#include "bms/tokenize.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

using enum Token_Type;

namespace {

Token_Type token_type_of(Literal_Type literal)
{
    switch (literal) {
    case Literal_Type::binary: return binary_literal;
    case Literal_Type::octal: return octal_literal;
    case Literal_Type::decimal: return decimal_literal;
    case Literal_Type::hexadecimal: return hexadecimal_literal;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown literal type.");
}

std::optional<Token_Type> keyword_by_name(std::string_view s) noexcept
{
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
        { "Void", keyword_void },
        { "while", keyword_while },
        { "break", keyword_break },
        { "continue", keyword_continue },
        { "return", keyword_return },
        { "function", keyword_function },
        { "true", keyword_true },
        { "false", keyword_false },
        { "requires", keyword_requires },
        { "static_assert", keyword_static_assert },
    };

    for (const auto [name, type] : types_by_name) {
        if (name == s) {
            return type;
        }
    }

    return std::nullopt;
}

std::optional<Token_Type> match_fixed_length_token(std::string_view s) noexcept
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
        }
        return less_than;
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

    Tokenize_Result(Size length, Token_Type type)
        : length(length)
        , type(type)
    {
        BIT_MANIPULATION_ASSERT(length != 0);
    }
};

/// @brief A helper class which manages some shared state for tokenization, and performs it.
struct Tokenizer {
    const std::string_view m_source;
    Local_Source_Span pos = {};

    Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out);

    Result<Tokenize_Result, Tokenize_Error> try_match(std::string_view s) const;

    void advance_position_by(Size length)
    {
        BIT_MANIPULATION_ASSERT(pos.begin + length < m_source.length());
        for (Size i = 0; i < length; ++i) {
            switch (m_source[pos.begin]) {
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
    }
};

Result<void, Tokenize_Error> Tokenizer::tokenize(std::pmr::vector<Token>& out)
{
    pos = {};
    while (pos.begin != m_source.length()) {
        BIT_MANIPULATION_ASSERT(pos.begin < m_source.length());

        std::string_view remainder = m_source.substr(pos.begin);

        if (const Size whitespace_length = remainder.find_first_not_of(" \r\t\n")) {
            if (whitespace_length == std::string_view::npos) {
                break;
            }
            advance_position_by(whitespace_length);
            remainder = remainder.substr(whitespace_length);
        }

        Result<Tokenize_Result, Tokenize_Error> result = try_match(remainder);
        if (!result) {
            return result.error();
        }
        pos.length = result->length;
        out.push_back({ pos, result->type });

        if (result->type != Token_Type::block_comment) {
            // Special case for all tokens that aren't multi-line.
            // We could just advance the position as usual, but it is much cheaper if we know
            // that there are no newline characters anyway.
            pos = pos.to_right(result->length);
            continue;
        }
        advance_position_by(result->length);
    }

    return {};
}

Result<Tokenize_Result, Tokenize_Error> Tokenizer::try_match(std::string_view s) const
{
    BIT_MANIPULATION_ASSERT(!s.empty());
    BIT_MANIPULATION_ASSERT(!is_space(s[0]));

    if (const std::optional<Comment_Match> m = match_line_comment(s)) {
        return Tokenize_Result { m->length, line_comment };
    }

    if (const std::optional<Comment_Match> m = match_block_comment(s)) {
        if (!m->is_terminated) {
            return Tokenize_Error { Tokenize_Error_Code::unterminated_comment, pos };
        }
        return Tokenize_Result { m->length, block_comment };
    }

    if (const Size length = match_identifier(s)) {
        if (const std::optional<Token_Type> keyword = keyword_by_name(s.substr(0, length))) {
            return Tokenize_Result { length, *keyword };
        }
        return Tokenize_Result { length, identifier };
    }

    if (const std::optional<Token_Type> type = match_fixed_length_token(s)) {
        return Tokenize_Result { token_type_length(*type), *type };
    }

    const Literal_Match_Result literal_match = match_integer_literal(s);
    if (literal_match) {
        const Size digits = literal_match.length;
        const bool attempted_integer_suffix = s.length() > digits
            && identifier_characters.find(s[digits]) != std::string_view::npos;
        if (attempted_integer_suffix) {
            return Tokenize_Error { Tokenize_Error_Code::integer_suffix, pos.to_right(digits) };
        }
        return Tokenize_Result { digits, token_type_of(literal_match.type) };
    }
    if (literal_match.status == Literal_Match_Status::no_digits_following_prefix) {
        return Tokenize_Error { Tokenize_Error_Code::no_digits_following_integer_prefix,
                                pos.to_right(2) };
    }

    return Tokenize_Error { Tokenize_Error_Code::illegal_character, pos };
}

} // namespace

Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out, std::string_view source)
{
    Tokenizer tokenizer { source };
    return tokenizer.tokenize(out);
}

} // namespace bit_manipulation::bms