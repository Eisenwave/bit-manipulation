#ifndef BIT_MANIPULATION_BMS_TOKENIZE_ERROR_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_ERROR_HPP

#include "common/assert.hpp"
#include "common/source_position.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Tokenize_Error_Code : Default_Underlying {
    /// @brief An illegal character was read.
    illegal_character,
    /// @brief No digits following an integer prefix, such as "0x" on its own.
    no_digits_following_integer_prefix,
    /// @brief An integer literal was suffixed by an identifier character, e.g. `123x`.
    integer_suffix,
    /// @brief An opening block comment has no closing asterisk and slash.
    unterminated_comment,
    /// @brief A string literal has an opening but no closing quote.
    unterminated_string,
};

[[nodiscard]] constexpr std::string_view name_of(Tokenize_Error_Code e)
{
    using enum Tokenize_Error_Code;
    switch (e) {
        BIT_MANIPULATION_ENUM_STRING_CASE(illegal_character);
        BIT_MANIPULATION_ENUM_STRING_CASE(no_digits_following_integer_prefix);
        BIT_MANIPULATION_ENUM_STRING_CASE(integer_suffix);
        BIT_MANIPULATION_ENUM_STRING_CASE(unterminated_comment);
        BIT_MANIPULATION_ENUM_STRING_CASE(unterminated_string);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

struct Tokenize_Error {
    Tokenize_Error_Code code;
    Local_Source_Position pos;
};

} // namespace bit_manipulation::bms

#endif
