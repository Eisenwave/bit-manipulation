#ifndef BIT_MANIPULATION_CODE_SPAN_TYPE_HPP
#define BIT_MANIPULATION_CODE_SPAN_TYPE_HPP

#include <string_view>

#include "common/assert.hpp"
#include "common/fwd.hpp"

namespace bit_manipulation {

/// @brief The type of a code span in a syntax-highlighted document.
/// While languages have many more tokens, these all fall into the listed handful categories
/// for the purpose of syntax highlighting.
enum struct Code_Span_Type : Default_Underlying {
    text,
    identifier,
    type_name,
    variable_name,
    function_name,
    annotation_name,
    bracket,
    number,
    string,
    comment,
    operation,
    punctuation,
    keyword,
    boolean_literal,
    error,
    diagnostic_text,
    diagnostic_error_text,
    diagnostic_code_position,
    diagnostic_error,
    diagnostic_warning,
    diagnostic_note,
    diagnostic_line_number,
    diagnostic_punctuation,
    diagnostic_position_indicator,
    diagnostic_code_citation,
    diagnostic_internal_error_notice,
    diagnostic_operand,
    diagnostic_operator,
    diagnostic_tag,
    diagnostic_attribute,
    diagnostic_internal,
    diagnostic_escape
};

} // namespace bit_manipulation

#endif
