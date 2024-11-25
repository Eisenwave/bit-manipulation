#ifndef BIT_MANIPULATION_FWD_HPP
#define BIT_MANIPULATION_FWD_HPP

#include "common/config.hpp"

namespace bit_manipulation {

enum struct Literal_Type : Default_Underlying;

struct Local_Source_Position;
struct Local_Source_Span;
struct Source_Span;
struct Source_Position;

template <typename... Ts>
struct Variant;

} // namespace bit_manipulation

#endif
