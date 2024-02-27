#include "bmd/bms_to_html.hpp"
#include "bmd/html_writer.hpp"

#include "bms/parse.hpp"
#include "bms/tokenize.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bmd {

namespace {

constexpr std::string_view token_type_tag(bms::Token_Type type)
{
    using enum bms::Token_Type;
    switch (type) {
    case identifier: return "c-idn";

    case left_parenthesis:
    case right_parenthesis:
    case left_brace:
    case right_brace: return "c-bra";

    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: return "c-int";

    case block_comment:
    case line_comment: return "c-com";

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
    case logical_not:
    case right_arrow: return "c-opr";

    case double_right_arrow: return "c-err";

    case dot:
    case colon:
    case comma:
    case semicolon: return "c-pun";

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
    case keyword_static_assert: return "c-key";

    case keyword_true:
    case keyword_false: return "c-bol";

    case keyword_uint:
    case keyword_int:
    case keyword_bool:
    case keyword_void: return "c-typ";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

void write_source_gap(HTML_Writer& out,
                      const Local_Source_Span& first,
                      const Local_Source_Span& second)
{
    BIT_MANIPULATION_ASSERT(first.line < second.line
                            || (first.line == second.line && first.column <= second.column));

    if (first.line != second.line) {
        out.write_whitespace('\n', second.line - first.line);
        if (second.column != 0) {
            out.write_whitespace(' ', second.column);
        }
    }
    else if (first.column != second.column) {
        out.write_whitespace(' ', second.column - first.end_column());
    }
}

void tokens_to_html(HTML_Writer& out, std::span<const bms::Token> tokens, std::string_view code)
{
    for (Size i = 0; i < tokens.size(); ++i) {
        if (i != 0) {
            write_source_gap(out, tokens[i - 1].pos, tokens[i].pos);
        }

        const Tag_Properties tag { token_type_tag(tokens[i].type), Formatting_Style::pre };
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

    Result<bms::Parsed_Program, bms::Parse_Error> p = bms::parse(tokens, code, memory);
    if (!p) {
        tokens_to_html(out, tokens, code);
        return Bms_Error { p.error() };
    }

    // TODO: imbue tokens with identifier information if parsing succeeded
    tokens_to_html(out, tokens, code);
    return {};
}

} // namespace bit_manipulation::bmd
