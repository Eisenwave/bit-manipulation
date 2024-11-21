#ifndef BIT_MANIPULATION_VISIT_HPP
#define BIT_MANIPULATION_VISIT_HPP

#include <type_traits>
#include <variant>

#include "common/assert.hpp"
#include "common/config.hpp"

/*
This header contains a drop-in replacement for std::visit.
This is necessary for two reasons:

1. Some standard library implementations are rather bloated and provide poor performance.
   We want something lightweight which is guaranteed to be as fast as simple switch statement.

2. Even if std::visit gets optimized nicely, its use tends to add a lot of stack frames, which is
   annoying for debugging.
   The custom implementation is as shallow as possible.

*/

namespace bit_manipulation {

namespace detail {

template <typename T>
inline constexpr Size variant_like_size_v
    = decltype([]<typename... Ts>(std::variant<Ts...>&)
                   -> std::integral_constant<std::size_t, sizeof...(Ts)> {}(
                       std::declval<T&>()))::value;

} // namespace detail

#define BIT_MANIPULATION_VISIT_CASE(...)                                                           \
    case __VA_ARGS__: return static_cast<F&&>(f)(::std::get<__VA_ARGS__>(static_cast<V&&>(v)))

template <typename F, typename V>
constexpr decltype(auto) fast_visit(F&& f, V&& v) = delete;

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 1)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    return static_cast<F&&>(f)(::std::get<0>(static_cast<V&&>(v)));
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 2)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 3)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 4)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 5)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
        BIT_MANIPULATION_VISIT_CASE(4);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 13)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
        BIT_MANIPULATION_VISIT_CASE(4);
        BIT_MANIPULATION_VISIT_CASE(5);
        BIT_MANIPULATION_VISIT_CASE(6);
        BIT_MANIPULATION_VISIT_CASE(7);
        BIT_MANIPULATION_VISIT_CASE(8);
        BIT_MANIPULATION_VISIT_CASE(9);
        BIT_MANIPULATION_VISIT_CASE(10);
        BIT_MANIPULATION_VISIT_CASE(11);
        BIT_MANIPULATION_VISIT_CASE(12);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 20)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
        BIT_MANIPULATION_VISIT_CASE(4);
        BIT_MANIPULATION_VISIT_CASE(5);
        BIT_MANIPULATION_VISIT_CASE(6);
        BIT_MANIPULATION_VISIT_CASE(7);
        BIT_MANIPULATION_VISIT_CASE(8);
        BIT_MANIPULATION_VISIT_CASE(9);
        BIT_MANIPULATION_VISIT_CASE(10);
        BIT_MANIPULATION_VISIT_CASE(11);
        BIT_MANIPULATION_VISIT_CASE(12);
        BIT_MANIPULATION_VISIT_CASE(13);
        BIT_MANIPULATION_VISIT_CASE(14);
        BIT_MANIPULATION_VISIT_CASE(15);
        BIT_MANIPULATION_VISIT_CASE(16);
        BIT_MANIPULATION_VISIT_CASE(17);
        BIT_MANIPULATION_VISIT_CASE(18);
        BIT_MANIPULATION_VISIT_CASE(19);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 21)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
        BIT_MANIPULATION_VISIT_CASE(4);
        BIT_MANIPULATION_VISIT_CASE(5);
        BIT_MANIPULATION_VISIT_CASE(6);
        BIT_MANIPULATION_VISIT_CASE(7);
        BIT_MANIPULATION_VISIT_CASE(8);
        BIT_MANIPULATION_VISIT_CASE(9);
        BIT_MANIPULATION_VISIT_CASE(10);
        BIT_MANIPULATION_VISIT_CASE(11);
        BIT_MANIPULATION_VISIT_CASE(12);
        BIT_MANIPULATION_VISIT_CASE(13);
        BIT_MANIPULATION_VISIT_CASE(14);
        BIT_MANIPULATION_VISIT_CASE(15);
        BIT_MANIPULATION_VISIT_CASE(16);
        BIT_MANIPULATION_VISIT_CASE(17);
        BIT_MANIPULATION_VISIT_CASE(18);
        BIT_MANIPULATION_VISIT_CASE(19);
        BIT_MANIPULATION_VISIT_CASE(20);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(detail::variant_like_size_v<std::remove_cvref_t<V>> == 22)
constexpr decltype(auto) fast_visit(F&& f, V&& v)
{
    if (v.valueless_by_exception()) {
        throw std::bad_variant_access();
    }
    switch (v.index()) {
        BIT_MANIPULATION_VISIT_CASE(0);
        BIT_MANIPULATION_VISIT_CASE(1);
        BIT_MANIPULATION_VISIT_CASE(2);
        BIT_MANIPULATION_VISIT_CASE(3);
        BIT_MANIPULATION_VISIT_CASE(4);
        BIT_MANIPULATION_VISIT_CASE(5);
        BIT_MANIPULATION_VISIT_CASE(6);
        BIT_MANIPULATION_VISIT_CASE(7);
        BIT_MANIPULATION_VISIT_CASE(8);
        BIT_MANIPULATION_VISIT_CASE(9);
        BIT_MANIPULATION_VISIT_CASE(10);
        BIT_MANIPULATION_VISIT_CASE(11);
        BIT_MANIPULATION_VISIT_CASE(12);
        BIT_MANIPULATION_VISIT_CASE(13);
        BIT_MANIPULATION_VISIT_CASE(14);
        BIT_MANIPULATION_VISIT_CASE(15);
        BIT_MANIPULATION_VISIT_CASE(16);
        BIT_MANIPULATION_VISIT_CASE(17);
        BIT_MANIPULATION_VISIT_CASE(18);
        BIT_MANIPULATION_VISIT_CASE(19);
        BIT_MANIPULATION_VISIT_CASE(20);
        BIT_MANIPULATION_VISIT_CASE(21);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

} // namespace bit_manipulation

#endif
