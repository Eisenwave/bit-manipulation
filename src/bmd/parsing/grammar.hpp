#ifndef BIT_MANIPULATION_BMD_GRAMMAR_HPP
#define BIT_MANIPULATION_BMD_GRAMMAR_HPP

#include <string_view>

#include "common/assert.hpp"
#include "common/config.hpp"

namespace bit_manipulation::bmd {

// DISCLAIMER: THIS GRAMMAR IS OUT-OF-DATE

// clang-format off
/// @see grammar.ebnf
enum struct Grammar_Rule : Default_Underlying {
    document,
    content,
    paragraph,
    paragraph_break,
    text,
    directive,
    arguments,
    block,
    raw_content,
    argument,
    value,
    binary_literal,
    octal_literal,
    decimal_literal,
    hexadecimal_literal,
    identifier,
    blank,
};
// clang-format on

[[nodiscard]] inline std::string_view grammar_rule_name(Grammar_Rule rule)
{
    using enum Grammar_Rule;
    switch (rule) {
    case document: return "document";
    case content: return "content";
    case paragraph: return "paragraph";
    case paragraph_break: return "paragraph_break";
    case text: return "text";
    case directive: return "directive";
    case arguments: return "arguments";
    case block: return "block";
    case raw_content: return "raw_content";
    case argument: return "argument";
    case value: return "value";
    case binary_literal: return "binary_literal";
    case octal_literal: return "octal_literal";
    case decimal_literal: return "decimal_literal";
    case hexadecimal_literal: return "hexadecimal_literal";
    case identifier: return "identifier";
    case blank: return "blank";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid grammar rule");
}

} // namespace bit_manipulation::bmd

#endif
