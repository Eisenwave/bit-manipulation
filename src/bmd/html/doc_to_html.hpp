#ifndef BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP

#include <span>
#include <string_view>

#include "common/result.hpp"
#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Document_Error_Code : Default_Underlying {
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
    /// @brief The language of a `\code` (or other) directive is invalid.
    invalid_language,
    /// @brief The architecture of a `\instruction` (or other) directive is invalid.
    invalid_architecture,
    /// @brief A string attribute was required, but a number was given.
    number_attribute_not_allowed,
    /// @brief Failed to highlight code.
    code_tokenization_failure,
};

struct Document_Error {
    Document_Error_Code code;
    Local_Source_Position pos;
};

struct Document_Options {
    Size indent_width;
    std::span<const std::string_view> stylesheets {};
};

/// @brief Converts the given parsed document to an HTML string.
/// @param out the string to write to; the contents will be entirely replaced
/// @param document the document to convert
/// @param options the options
/// @param memory a source of temporary memory for subroutines such as BMS tokenization
[[nodiscard]] Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out,
                                                       const Parsed_Document& document,
                                                       Document_Options options,
                                                       std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
