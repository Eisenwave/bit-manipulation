
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
    case variable_name: return "c-iva";
    case function_name: return "c-ifn";
    case annotation_name: return "c-ian";
    case type_name: return "c-ity";
    case bracket: return "c-bra";
    case number: return "c-num";
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

    case html_preamble: return "c-html-pre";
    case html_comment: return "c-html-com";
    case html_tag_identifier: return "c-html-tag";
    case html_tag_bracket: return "c-html-bra";
    case html_attribute_key: return "c-html-aky";
    case html_attribute_equal: return "c-html-aeq";
    case html_attribute_value: return "c-html-avl";
    case html_inner_text: return "c-html-pre";
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
                out.write_inner_text(gap_text);
            }
        }
        const std::string_view id = code_span_type_tag(spans[i].type);
        out.open_tag(id);
        out.write_inner_text(string.get_text(spans[i]));
        out.close_tag(id);
    }
}

} // namespace bit_manipulation::bmd
