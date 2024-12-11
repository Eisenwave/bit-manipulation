#include "bms/parsing/parse.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/token_type.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "bmd/codegen/code_span_type.hpp"
#include "bmd/html/bms_to_html.hpp"
#include "bmd/html/html_writer.hpp"

namespace bit_manipulation::bmd {

namespace {

constexpr Code_Span_Type categorize_token_type(bms::Token_Type type)
{
    using enum bms::Token_Type;
    BIT_MANIPULATION_ASSERT(type != eof);

    switch (type) {
    case identifier: return Code_Span_Type::identifier;

    case left_parenthesis:
    case right_parenthesis:
    case left_brace:
    case right_brace: return Code_Span_Type::bracket;

    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: return Code_Span_Type::number;

    case string_literal: return Code_Span_Type::string;

    case block_comment:
    case line_comment: return Code_Span_Type::comment;

    case assign:
    case equals:
    case not_equals:
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal:
    case shift_left:
    case shift_right:
    case bitwise_and:
    case bitwise_or:
    case bitwise_not:
    case bitwise_xor:
    case logical_and:
    case logical_or:
    case logical_not: return Code_Span_Type::operation;

    case double_right_arrow: return Code_Span_Type::error;

    case at:
    case dot:
    case colon:
    case comma:
    case semicolon:
    case right_arrow: return Code_Span_Type::punctuation;

    case keyword_as:
    case keyword_let:
    case keyword_const:
    case keyword_function:
    case keyword_while:
    case keyword_if:
    case keyword_else:
    case keyword_requires:
    case keyword_return:
    case keyword_break:
    case keyword_continue:
    case keyword_static_assert: return Code_Span_Type::keyword;

    case keyword_true:
    case keyword_false: return Code_Span_Type::boolean_literal;

    case keyword_uint:
    case keyword_int:
    case keyword_bool:
    case keyword_void: return Code_Span_Type::type_name;

    case eof: break;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

void tokens_to_html(HTML_Writer& out, std::span<const bms::Token> tokens, std::string_view code)
{
    for (Size i = 0; i < tokens.size(); ++i) {
        if (i != 0) {
            out.write_source_gap(tokens[i - 1].pos, tokens[i].pos, Formatting_Style::pre);
        }

        const Code_Span_Type category = categorize_token_type(tokens[i].type);
        const Tag_Properties tag { code_span_type_tag(category), Formatting_Style::pre };
        out.begin_tag(tag);
        const std::string_view text = code.substr(tokens[i].pos.begin, tokens[i].pos.length);
        out.write_inner_text(text, Formatting_Style::pre);
        out.end_tag(tag);
    }
}

} // namespace

Result<void, Bms_Error>
bms_inline_code_to_html(HTML_Writer& out, std::string_view code, std::pmr::memory_resource* memory)
{
    constexpr auto style = Formatting_Style::in_line;

    std::pmr::vector<bms::Token> tokens(memory);
    Result<void, bms::Tokenize_Error> t = bms::tokenize(tokens, code);
    if (!t) {
        out.write_inner_text(code, style);
        return Bms_Error { t.error() };
    }

    bms::Parsed_Program parsed(code, memory);
    Result<void, bms::Parse_Error> p = bms::parse(parsed, tokens);
    if (!p) {
        tokens_to_html(out, tokens, code);
        return Bms_Error { p.error() };
    }

    // TODO: imbue tokens with identifier information if parsing succeeded
    tokens_to_html(out, tokens, code);
    return {};
}

} // namespace bit_manipulation::bmd
