#ifndef BIT_MANIPULATION_META_HPP
#define BIT_MANIPULATION_META_HPP

#include <type_traits>

namespace bit_manipulation {

template <typename T, typename U>
using const_like_t = std::conditional_t<std::is_const_v<U>, const T, T>;

} // namespace bit_manipulation

#endif
