#ifndef BIT_MANIPULATION_COMPILATION_STAGE_HPP
#define BIT_MANIPULATION_COMPILATION_STAGE_HPP

#include "bms/fwd.hpp"

namespace bit_manipulation {

enum struct Testing_Stage : Default_Underlying { load_file, tokenize, parse, analyze, introspect };

constexpr auto operator<=>(Testing_Stage a, Testing_Stage b)
{
    return static_cast<Default_Underlying>(a) <=> static_cast<Default_Underlying>(b);
}

} // namespace bit_manipulation

#endif
