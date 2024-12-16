#ifndef BIT_MANIPULATION_TO_STRING_HPP
#define BIT_MANIPULATION_TO_STRING_HPP

#include <string>

#include "common/to_chars.hpp"

namespace bit_manipulation {

using std::to_string;

#ifdef BIT_MANIPULATION_HAS_INT_128
[[nodiscard]] inline std::string to_string(Int128 x)
{
    return std::string(to_characters(x).as_string());
}

[[nodiscard]] inline std::string to_string(Uint128 x)
{
    return std::string(to_characters(x).as_string());
}
#endif

} // namespace bit_manipulation

#endif
