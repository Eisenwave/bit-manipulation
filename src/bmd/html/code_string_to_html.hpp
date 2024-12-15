#ifndef BIT_MANIPULATION_BMD_CODE_STRING_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_CODE_STRING_TO_HTML_HPP

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

[[nodiscard]] std::string_view code_span_type_tag(Code_Span_Type);

void code_string_to_html(HTML_Writer& out, const Code_String& string);

} // namespace bit_manipulation::bmd

#endif
