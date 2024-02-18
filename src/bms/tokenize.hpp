#ifndef BIT_MANIPULATION_BMS_TOKENIZE_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_HPP

#include <string_view>
#include <vector>

#include "config.hpp"
#include "result.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Tokenize_Error_Code : Default_Underlying {
    /// @brief An illegal character was read.
    illegal_character,
    /// @brief An integer literal was suffixed by an identifier character, e.g. `123x`.
    integer_suffix,
    /// @brief An opening block comment has no closing asterisk and slash.
    unterminated_comment,
};

struct Tokenize_Error {
    Tokenize_Error_Code code;
    Local_Source_Position pos;
};

Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out,
                                      std::string_view source) noexcept;

} // namespace bit_manipulation::bms

#endif