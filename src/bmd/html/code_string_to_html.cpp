
#include "common/code_span_type.hpp"
#include "common/code_string.hpp"

#include "bmd/html/code_string_to_html.hpp"
#include "bmd/html/html_writer.hpp"

namespace bit_manipulation::bmd {

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
