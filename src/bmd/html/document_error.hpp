#ifndef BIT_MANIPULATION_DOCUMENT_ERROR_HPP
#define BIT_MANIPULATION_DOCUMENT_ERROR_HPP

#include "common/source_position.hpp"

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

} // namespace bit_manipulation::bmd

#endif
