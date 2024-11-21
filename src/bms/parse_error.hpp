#ifndef BIT_MANIPULATION_BMS_PARSE_ERROR_HPP
#define BIT_MANIPULATION_BMS_PARSE_ERROR_HPP

#include <span>

#include "bms/fwd.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

/// @brief An error that occurs during parsing.
struct Parse_Error {
    /// @brief The rule that failed to match.
    /// While there are multiple expansions, the BMS parser is completely deterministic given
    /// arbitrary lookahead in a few isolated instances.
    /// For example, if `let` appears in code, the rule to match would be `let_declaration`.
    Grammar_Rule fail_rule;
    /// @brief A list of `Token_Type`s which, if they appeared, could have resulted in a match.
    /// For example, following a `keyword_let`, this would include an `identifier`.
    std::span<const Token_Type> expected_tokens;
    /// @brief The token at which parsing failed.
    Token fail_token;
};

} // namespace bit_manipulation::bms

#endif
