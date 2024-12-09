#ifndef BIT_MANIPULATION_META_HPP
#define BIT_MANIPULATION_META_HPP

#include <type_traits>

namespace bit_manipulation {

template <typename T, bool C>
using const_if_t = std::conditional_t<C, const T, T>;

template <typename T, typename U>
using const_like_t = const_if_t<T, std::is_const_v<U>>;

template <typename>
inline constexpr bool dependent_false = false;

} // namespace bit_manipulation

#endif
