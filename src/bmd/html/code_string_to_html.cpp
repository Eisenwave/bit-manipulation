
#include "common/code_span_type.hpp"
#include "common/code_string.hpp"

#include "bmd/html/code_string_to_html.hpp"
#include "bmd/html/html_writer.hpp"

namespace bit_manipulation::bmd {

/// @brief Returns the HTML (custom) tag name corresponding to the given `type`.
[[nodiscard]] std::string_view code_span_type_tag(Code_Span_Type type)
{
    using enum Code_Span_Type;
    switch (type) {
    case text: return "c-txt";
    case identifier: return "c-idn";
    case type_name: return "c-typ";
    case bracket: return "c-bra";
    case number: return "c-int";
    case string: return "c-str";
    case comment: return "c-com";
    case operation: return "c-opr";
    case punctuation: return "c-pun";
    case keyword: return "c-key";
    case boolean_literal: return "c-bol";
    case error: return "c-err";
    case diagnostic_text: return "c-dtx";
    case diagnostic_error_text: return "c-det";
    case diagnostic_code_position: return "c-dcp";
    case diagnostic_error: return "c-der";
    case diagnostic_warning: return "c-dwr";
    case diagnostic_note: return "c-dnt";
    case diagnostic_line_number: return "c-dln";
    case diagnostic_punctuation: return "c-dpc";
    case diagnostic_position_indicator: return "c-dpi";
    case diagnostic_code_citation: return "c-dcc";
    case diagnostic_internal_error_notice: return "c-die";
    case diagnostic_operand: return "c-dor";
    case diagnostic_operator: return "c-dop";
    case diagnostic_tag: return "c-dtg";
    case diagnostic_attribute: return "c-dat";
    case diagnostic_internal: return "c-dit";
    case diagnostic_escape: return "c-des";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

void code_string_to_html(HTML_Writer& out, const Code_String& string)
{
    auto spans = string.begin();

    const auto n = Difference(string.get_span_count());
    for (Difference i = 0; i < n; ++i) {
        if (i != 0) {
            const Size previous_end = spans[i - 1].end();
            if (spans[i].begin != previous_end) {
                const std::string_view gap_text
                    = string.get_text().substr(previous_end, spans[i].begin - previous_end);
                out.write_inner_text(gap_text, Formatting_Style::pre);
            }
        }
        const Tag_Properties tag { code_span_type_tag(spans[i].type), Formatting_Style::pre };
        out.begin_tag(tag);
        out.write_inner_text(string.get_text(spans[i]), Formatting_Style::pre);
        out.end_tag(tag);
    }
}

} // namespace bit_manipulation::bmd
