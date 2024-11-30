#ifndef BIT_MANIPULATION_BMD_GRAMMAR_HPP
#define BIT_MANIPULATION_BMD_GRAMMAR_HPP

#include <string_view>

#include "common/assert.hpp"
#include "common/config.hpp"

namespace bit_manipulation::bmd {

// DISCLAIMER: THIS GRAMMAR IS OUT-OF-DATE

// clang-format off
enum struct Grammar_Rule : Default_Underlying {
    document, // = [blank | paragraph_break], [content];
    content, // = paragraph, { paragraph_break, [paragraph] };
    paragraph, // = { (directive | text), [blank] };
    paragraph_break, // = (* whitespace/comment sequence containing a whitespace line outside
                     // comments *);
    text, // = (* longest sequence of characters that does not include a "\\" (other than for escape
          // sequences) or paragraph_break *);
    directive, // = "\\", identifier, [arguments], [block];
    arguments, // = "[", { [blank], argument, [blank], [","] }, [blank] "]";
    block, // = "{", raw_content | ([blank | paragraph_break], content), "}";
    raw_content, // = (* longest brace-balanced sequence, including whitespace and comments *)
    argument, // = identifier, [blank], "=", [blank], value;
    value, // = binary_literal | octal_literal | decimal_literal | hexadecimal_literal | identifier;
    binary_literal, // = "0b", ("0" | "1"), {"0" | "1"};
    octal_literal, // = "0", (* octal digit *), {(* octal digit *)};
    decimal_literal, // = (* decimal digit *), {(* decimal digit *)};
    hexadecimal_literal, // "0x", (* hexadecimal digit *), {(* hexadecimal digit *)};
    identifier, // /[_a-zA-Z][_a-zA-Z0-9]*/;
    blank, // = (* C89-style comment, C99-style comment, or whitespace sequence *)
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
