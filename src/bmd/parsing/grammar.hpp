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

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule);

} // namespace bit_manipulation::bmd

#endif
