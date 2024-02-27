#ifndef BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP

#include "common/result.hpp"
#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Document_Error_Code {
    /// @brief A directive is not allowed in a specific context.
    /// For example, `\i` is not directly allowed inside a `\meta` directive.
    directive_not_allowed,
    /// @brief A `\meta` directive was found in a place other than the start of the file.
    /// Only whitespace or comments can precede it.
    meta_not_at_start_of_file,
    /// @brief The HTML_Writer was misused. This is an internal error.
    writer_misuse,
    /// @brief An entry in the `\\meta` directive was defined twice.
    duplicate_meta_entry,
};

struct Document_Error {
    Document_Error_Code code;
    Local_Source_Position pos;
};

/// @brief Converts the given parsed document to an HTML string.
/// @param out the string to write to; the contents will be entirely replaced
/// @param document the document to convert
/// @param indent_width the indent width
Result<void, Document_Error>
doc_to_html(HTML_Token_Consumer& out, const Parsed_Document& document, Size indent_width);

} // namespace bit_manipulation::bmd

#endif
