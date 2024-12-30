#ifndef BIT_MANIPULATION_COMPILATION_STAGE_HPP
#define BIT_MANIPULATION_COMPILATION_STAGE_HPP

#include "common/config.hpp"

namespace bit_manipulation {

enum struct BMS_Stage : Default_Underlying { //
    load_file,
    tokenize,
    parse,
    analyze,
    introspect
};

constexpr auto operator<=>(BMS_Stage a, BMS_Stage b)
{
    return static_cast<Default_Underlying>(a) <=> static_cast<Default_Underlying>(b);
}

enum struct BMD_Stage : Default_Underlying { //
    load_file,
    parse,
    process
};

constexpr auto operator<=>(BMD_Stage a, BMD_Stage b)
{
    return static_cast<Default_Underlying>(a) <=> static_cast<Default_Underlying>(b);
}

} // namespace bit_manipulation

#endif
