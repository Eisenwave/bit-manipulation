#ifndef BIT_MANIPULATION_BMS_PARSE_ERROR_HPP
#define BIT_MANIPULATION_BMS_PARSE_ERROR_HPP

#include <span>

#include "bms/fwd.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

} // namespace bit_manipulation::bms

#endif
