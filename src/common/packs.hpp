#ifndef BIT_MANIPULATION_PACKS_HPP
#define BIT_MANIPULATION_PACKS_HPP

#include "common/fwd.hpp"

namespace bit_manipulation {

namespace detail {

template <Size I, typename... Ts>
struct Pack_At_Index { };

template <typename Head, typename... Tail>
struct Pack_At_Index<0, Head, Tail...> {
    using type = Head;
};

template <Size I, typename Head, typename... Tail>
struct Pack_At_Index<I, Head, Tail...> : Pack_At_Index<I - 1, Tail...> { };

template <typename T, typename... Ts>
consteval Size pack_first_index_of_impl()
{
    Size i = 0;
    Size result(-1);
    ((std::is_same_v<T, Ts> ? result = i, true : ++i, false) || ...);
    return result;
}

template <template <typename> typename Predicate, typename... Ts>
consteval Size pack_first_index_satisfying_impl()
{
    Size i = 0;
    Size result(-1);
    ((Predicate<Ts>::value ? result = i, true : ++i, false) || ...);
    return result;
}

template <typename T, typename... Ts>
consteval Size pack_last_index_of_impl()
{
    Size i = 0;
    Size result(-1);
    ((std::is_same_v<T, Ts> ? result = i++ : ++i), ...);
    return result;
}

} // namespace detail

template <Size I, typename... Ts>
using pack_at_index_t = typename detail::Pack_At_Index<I, Ts...>::type;

template <typename... Ts>
using pack_head_t = typename detail::Pack_At_Index<0, Ts...>::type;

template <typename T, typename... Ts>
inline constexpr Size pack_first_index_of_v = detail::pack_first_index_of_impl<T, Ts...>();

template <template <typename> typename Predicate, typename... Ts>
inline constexpr Size pack_first_index_satisfying_v
    = detail::pack_first_index_satisfying_impl<Predicate, Ts...>();

template <typename T, typename... Ts>
inline constexpr Size pack_last_index_of_v = detail::pack_last_index_of_impl<T, Ts...>();

} // namespace bit_manipulation

#endif
