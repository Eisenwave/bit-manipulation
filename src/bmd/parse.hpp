#ifndef BIT_MANIPULATION_BMD_TOKENS_HPP
#define BIT_MANIPULATION_BMD_TOKENS_HPP

#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "common/config.hpp"
#include "common/result.hpp"
#include "common/variant.hpp"

#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Parse_Error_Code : Default_Underlying {
    /// @brief An illegal character was read.
    unexpected_character,
    /// @brief An opening block comment has no closing asterisk and slash.
    unterminated_comment,
    invalid_integer_literal,
    unexpected_eof,
    duplicate_argument,
    integer_suffix,
    invalid_directive,
    directive_must_be_empty,
    paragraph_break_in_span,
    directive_in_text_span,
    text_in_directive_list,
    directive_not_allowed,
};

struct Parse_Error {
    Parse_Error_Code code;
    Grammar_Rule rule;
    Local_Source_Position pos;
};

struct Parsed_Document {
    std::string_view source;
    ast::Some_Node* root_node;

    [[nodiscard]] std::string_view extract(const Local_Source_Span& span) const
    {
        BIT_MANIPULATION_ASSERT(span.begin < source.length());
        BIT_MANIPULATION_ASSERT(span.end() <= source.length());
        return source.substr(span.begin, span.length);
    }
};

Result<Parsed_Document, Parse_Error> parse(std::string_view source,
                                           std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
