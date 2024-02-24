#ifndef BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP

#include "common/result.hpp"
#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Document_Error_Code {
    /// @brief An unknown directive was used, e.g. `\xxx`
    unknown_directive,
    /// @brief The HTML_Writer was misused. This is an internal error.
    writer_misuse
};

struct Document_Error {
    Document_Error_Code code;
    Local_Source_Position pos;
};

/// @brief Converts the given parsed document to an HTML string.
/// @param out the string to write to; the contents will be entirely replaced
/// @param document the document to convert
Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out, const Parsed_Document& document);

} // namespace bit_manipulation::bmd

#endif
