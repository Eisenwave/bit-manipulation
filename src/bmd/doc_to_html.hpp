#ifndef BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP

#include <string>

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

/// @brief Converts the given parsed document to an HTML string.
/// @param out the string to write to; the contents will be entirely replaced
/// @param document the document to convert
void doc_to_html(std::pmr::string& out, const Parsed_Document& document);

} // namespace bit_manipulation::bmd

#endif
