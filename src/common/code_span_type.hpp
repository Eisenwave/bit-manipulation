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
    identifier,
    type_name,
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
    diagnostic_operator
};

/// @brief Returns the HTML (custom) tag name corresponding to the given `type`.
[[nodiscard]] constexpr std::string_view code_span_type_tag(Code_Span_Type type)
{
    using enum Code_Span_Type;
    switch (type) {
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
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

} // namespace bit_manipulation

#endif
