#ifndef BIT_MANIPULATION_BMD_TOKEN_TYPE_HPP
#define BIT_MANIPULATION_BMD_TOKEN_TYPE_HPP

#include "common/config.hpp"

namespace bit_manipulation::bmd {

enum struct HTML_Token_Type : Default_Underlying {
    whitespace,
    preamble,
    comment,
    tag_identifier,
    tag_bracket,
    attribute_key,
    attribute_equal,
    attribute_comma,
    attribute_quote,
    attribute_value,
    inner_text,
};

} // namespace bit_manipulation::bmd

#endif
