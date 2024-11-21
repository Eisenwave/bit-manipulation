#ifndef BIT_MANIPULATION_COMPILATION_STAGE_HPP
#define BIT_MANIPULATION_COMPILATION_STAGE_HPP

#include "bms/fwd.hpp"

namespace bit_manipulation {

enum struct Compilation_Stage : Default_Underlying { load_file, tokenize, parse, analyze };

constexpr auto operator<=>(Compilation_Stage a, Compilation_Stage b)
{
    return static_cast<Default_Underlying>(a) <=> static_cast<Default_Underlying>(b);
}

} // namespace bit_manipulation

#endif
