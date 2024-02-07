#ifndef BIT_MANIPULATION_VISIT_HPP
#define BIT_MANIPULATION_VISIT_HPP

#include <type_traits>
#include <variant>

#include "assert.hpp"

namespace bit_manipulation {

namespace detail {

template <typename T>
struct Is_Variant : std::false_type { };

template <typename... Ts>
struct Is_Variant<std::variant<Ts...>> : std::true_type { };

} // namespace detail

#define BIT_MANIPULATION_VISIT_CASE(...)                                                           \
    case __VA_ARGS__: return static_cast<F&&>(f)(std::get<__VA_ARGS__>(static_cast<V&&>(v)))

template <typename F, typename V>
constexpr decltype(auto) fast_visit(F&& f, V&& v) = delete;

template <typename F, typename V>
    requires(std::variant_size_v<std::remove_cvref_t<V>> == 11)
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
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
}

template <typename F, typename V>
    requires(std::variant_size_v<std::remove_cvref_t<V>> == 20)
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

} // namespace bit_manipulation

#endif