#ifndef BIT_MANIPULATION_BMD_CODE_SPAN_TYPE_HPP
#define BIT_MANIPULATION_BMD_CODE_SPAN_TYPE_HPP

#include <string_view>

#include "common/assert.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

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
};

/// @brief Returns the HTML (custom) tag name corresponding to the given `type`.
constexpr std::string_view code_span_type_tag(Code_Span_Type type)
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
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

} // namespace bit_manipulation::bmd

#endif
