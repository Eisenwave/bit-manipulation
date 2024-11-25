#ifndef BIT_MANIPULATION_VARIANT_HPP
#define BIT_MANIPULATION_VARIANT_HPP

#include <concepts>
#include <type_traits>
#include <utility>

#include "common/assert.hpp"
#include "common/fwd.hpp"
#include "common/packs.hpp"

namespace bit_manipulation {

struct Monostate { };

/// @brief A helper type which assists in building overload sets for variant visitors.
/// `Ignore` can be constructed from any other type and ignores the value if anything it is
/// constructed from.
struct Ignore {
    Ignore(auto&&) { }

    Ignore(const Ignore&) = delete;
    Ignore(Ignore&&) = delete;

    Ignore& operator=(const Ignore&) = delete;
    Ignore& operator=(Ignore&&) = delete;
};

template <class T, class U>
constexpr auto&& forward_like(U&& x) noexcept
{
    constexpr bool is_adding_const = std::is_const_v<std::remove_reference_t<T>>;
    if constexpr (std::is_lvalue_reference_v<T&&>) {
        if constexpr (is_adding_const)
            return std::as_const(x);
        else
            return static_cast<U&>(x);
    }
    else {
        if constexpr (is_adding_const)
            return std::move(std::as_const(x));
        else
            return std::move(x);
    }
}

template <typename T, typename U>
using const_like_t = std::conditional_t<std::is_const_v<U>, const T, T>;

#undef BIT_MANIPULATION_VISIT_CASE // FIXME: remove once migrated away from fast_visit
#define BIT_MANIPULATION_VISIT_CASE(...)                                                           \
    case __VA_ARGS__: return static_cast<F&&>(f)(get<__VA_ARGS__>(static_cast<V&&>(v)))

template <typename F, typename V>
    requires(std::remove_reference_t<V>::alternatives <= 24)
constexpr decltype(auto) visit(F&& f, V&& v)
{
    using VV = std::remove_reference_t<V>;
    if constexpr (VV::alternatives == 1) {
        return static_cast<F&&>(f)(::std::get<0>(static_cast<V&&>(v)));
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 2) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 3) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 4) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
            BIT_MANIPULATION_VISIT_CASE(3);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 5) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
            BIT_MANIPULATION_VISIT_CASE(3);
            BIT_MANIPULATION_VISIT_CASE(4);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 6) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
            BIT_MANIPULATION_VISIT_CASE(3);
            BIT_MANIPULATION_VISIT_CASE(4);
            BIT_MANIPULATION_VISIT_CASE(5);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 7) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
            BIT_MANIPULATION_VISIT_CASE(3);
            BIT_MANIPULATION_VISIT_CASE(4);
            BIT_MANIPULATION_VISIT_CASE(5);
            BIT_MANIPULATION_VISIT_CASE(6);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 8) {
        switch (v.index()) {
            BIT_MANIPULATION_VISIT_CASE(0);
            BIT_MANIPULATION_VISIT_CASE(1);
            BIT_MANIPULATION_VISIT_CASE(2);
            BIT_MANIPULATION_VISIT_CASE(3);
            BIT_MANIPULATION_VISIT_CASE(4);
            BIT_MANIPULATION_VISIT_CASE(5);
            BIT_MANIPULATION_VISIT_CASE(6);
            BIT_MANIPULATION_VISIT_CASE(7);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 9) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 10) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 11) {
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
    else if constexpr (std::remove_reference_t<V>::alternatives == 12) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 13) {
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
    else if constexpr (std::remove_reference_t<V>::alternatives == 14) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 15) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 16) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 17) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 18) {
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
            BIT_MANIPULATION_VISIT_CASE(17);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 19) {
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
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 20) {
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
    else if constexpr (std::remove_reference_t<V>::alternatives == 21) {
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
    else if constexpr (std::remove_reference_t<V>::alternatives == 22) {
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
    else if constexpr (std::remove_reference_t<V>::alternatives == 23) {
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
            BIT_MANIPULATION_VISIT_CASE(22);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
    else if constexpr (std::remove_reference_t<V>::alternatives == 24) {
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
            BIT_MANIPULATION_VISIT_CASE(22);
            BIT_MANIPULATION_VISIT_CASE(23);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("impossible variant index");
    }
}

#undef BIT_MANIPULATION_VISIT_CASE

namespace detail {

template <typename V, typename T>
struct Variant_Index_Of;

template <typename T, typename... Ts>
    requires(std::is_same_v<T, Ts> || ...)
struct Variant_Index_Of<Variant<Ts...>, T>
    : std::integral_constant<Size, pack_first_index_of_v<T, Ts...>> { };

template <typename V, typename T>
struct Has_Alternative;

template <typename T, typename... Ts>
struct Has_Alternative<Variant<Ts...>, T> : std::bool_constant<(std::is_same_v<T, Ts> || ...)> { };

template <typename V, Size I>
struct Variant_Alternative;

template <Size I, typename... Ts>
struct Variant_Alternative<Variant<Ts...>, I> {
    using type = pack_at_index_t<I, Ts...>;
};

struct Variant_Get_Impl {

    template <Size I, typename... Ts>
    [[nodiscard]] static pack_at_index_t<I, Ts...>* get_if(Variant<Ts...>* variant)
    {
        BIT_MANIPULATION_ASSERT(variant);
        return variant->m_index == I
            ? std::launder(reinterpret_cast<pack_at_index_t<I, Ts...>*>(variant->m_storage))
            : nullptr;
    }

    template <Size I, typename... Ts>
    [[nodiscard]] static const pack_at_index_t<I, Ts...>* get_if(const Variant<Ts...>* variant)
    {
        BIT_MANIPULATION_ASSERT(variant);
        return variant->m_index == I
            ? std::launder(reinterpret_cast<const pack_at_index_t<I, Ts...>*>(variant->m_storage))
            : nullptr;
    }

    template <Size I, typename V>
    [[nodiscard]] static decltype(auto) get(V&& variant)
    {
        using VV = std::remove_cvref_t<V>;
        static_assert(I < VV::alternatives);
        BIT_MANIPULATION_ASSERT(I == variant.m_index);

        using Result
            = const_like_t<typename Variant_Alternative<VV, I>::type, std::remove_reference_t<V>>;
        Result* result = std::launder(reinterpret_cast<Result*>(variant.m_storage));
        return forward_like<V>(*result);
    }
};

constexpr Size max_size(std::same_as<Size> auto... sizes)
{
    Size result = 0;
    ((sizes > result ? void(result = sizes) : void()), ...);
    return result;
}

static_assert(max_size(Size(1), Size(2), Size(3)) == Size(3));

} // namespace detail

template <typename V, typename T>
inline constexpr Size alternative_index_v = detail::Variant_Index_Of<V, T>::value;

template <typename V, typename T>
inline constexpr bool has_alternative_v = detail::Has_Alternative<V, T>::value;

template <typename V, Size I>
using alternative_t = typename detail::Variant_Alternative<V, I>::value;

template <typename... Ts>
struct Variant {
public:
    static_assert((std::is_object_v<Ts> && ...));

    static constexpr Size alternatives = sizeof...(Ts);

private:
    template <Size I>
    struct Default_Construct_Tag { };

    unsigned char m_index;
    alignas(Ts...) std::byte m_storage[detail::max_size(sizeof(Ts)...)];

public:
    Variant() noexcept(std::is_nothrow_default_constructible_v<pack_head_t<Ts...>>)
        requires(std::is_default_constructible_v<pack_head_t<Ts...>>)
        : m_index(0)
    {
        std::construct_at(reinterpret_cast<pack_head_t<Ts...>*>(m_storage));
    }

    Variant(Variant&& other) noexcept
        requires(std::is_nothrow_move_constructible_v<Ts> && ...)
        : m_index(other.m_index)
    {
        visit(
            [&]<typename T>(T&& x) { //
                std::construct_at(reinterpret_cast<T*>(m_storage), std::move(x));
            },
            std::move(other));
    }

    Variant(const Variant& other) noexcept((std::is_nothrow_copy_constructible_v<Ts> && ...))
        requires(std::is_copy_constructible_v<Ts> && ...)
        : m_index(other.m_index)
    {
        visit(
            [&]<typename T>(const T& x) { //
                std::construct_at(reinterpret_cast<T*>(m_storage), x);
            },
            other);
    }

    template <typename T>
    Variant(T&& other) noexcept(noexcept(T(std::forward<T>(other))))
        requires has_alternative_v<Variant, std::remove_cvref_t<T>>
        : m_index(pack_first_index_of_v<std::remove_cvref_t<T>, Ts...>)
    {
        std::construct_at(reinterpret_cast<std::remove_reference_t<T>*>(m_storage),
                          std::forward<T>(other));
    }

    ~Variant()
    {
        destroy();
    }

    Variant& operator=(Variant&& other) noexcept
        requires((std::is_nothrow_move_assignable_v<Ts> && ...)
                 && (std::is_nothrow_move_constructible_v<Ts> && ...))
    {
        if (m_index == other.m_index) {
            visit(
                [&]<typename T>(T&& x) { //
                    *std::launder(reinterpret_cast<T*>(m_storage)) = std::move(x);
                },
                std::move(other));
        }
        else {
            destroy();
            visit(
                [&]<typename T>(T&& x) { //
                    std::construct_at(reinterpret_cast<T*>(m_storage), std::move(x));
                },
                std::move(other));
            m_index = other.m_index;
        }
        return *this;
    }

    Variant&
    operator=(const Variant& other) noexcept((std::is_nothrow_copy_assignable_v<Ts> && ...)
                                             && (std::is_nothrow_copy_constructible_v<Ts> && ...))
        requires((std::is_copy_assignable_v<Ts> && ...)
                 && ((std::is_nothrow_copy_constructible_v<Ts>
                      || std::is_nothrow_move_constructible_v<Ts>)
                     && ...))
    {
        if (m_index == other.m_index) {
            visit(
                [&]<typename T>(const T& x) { //
                    *std::launder(reinterpret_cast<T*>(m_storage)) = x;
                },
                other);
        }
        else {
            visit(
                [&]<typename T>(const T& x) { //
                    if constexpr (std::is_nothrow_copy_constructible_v<T>) {
                        destroy();
                        std::construct_at(reinterpret_cast<T*>(m_storage), x);
                    }
                    else {
                        static_assert(std::is_nothrow_move_constructible_v<T>);
                        auto copy = x;
                        destroy();
                        std::construct_at(reinterpret_cast<T*>(m_storage), std::move(copy));
                    }
                },
                other);
            m_index = other.m_index;
        }
        return *this;
    }

    [[nodiscard]] Size index() const noexcept
    {
        return m_index;
    }

private:
    void destroy() noexcept
    {
        visit(
            [&]<typename T>(T& x) noexcept { //
                std::destroy_at(reinterpret_cast<T*>(std::addressof(x)));
            },
            *this);
    }

    friend struct detail::Variant_Get_Impl;
};

template <typename... Ts>
    requires(std::is_trivially_copyable_v<Ts> && ...)
struct Variant<Ts...> {
public:
    static_assert((std::is_object_v<Ts> && ...));

    static constexpr Size alternatives = sizeof...(Ts);

private:
    unsigned char m_index;
    alignas(Ts...) std::byte m_storage[detail::max_size(sizeof(Ts)...)];

public:
    Variant() noexcept(std::is_nothrow_default_constructible_v<pack_head_t<Ts...>>)
        requires(std::is_default_constructible_v<pack_head_t<Ts...>>)
        : m_index(0)
    {
        std::construct_at(reinterpret_cast<pack_head_t<Ts...>*>(m_storage));
    }

    template <typename T>
    Variant(T&& other) noexcept(noexcept(T(std::forward<T>(other))))
        requires has_alternative_v<Variant, std::remove_cvref_t<T>>
        : m_index(pack_first_index_of_v<std::remove_cvref_t<T>, Ts...>)
    {
        std::construct_at(reinterpret_cast<std::remove_reference_t<T>*>(m_storage),
                          std::forward<T>(other));
    }

    [[nodiscard]] Size index() const noexcept
    {
        return m_index;
    }

    friend struct detail::Variant_Get_Impl;
};

template <typename T, typename V>
    requires has_alternative_v<V, T>
[[nodiscard]] bool holds_alternative(const V& v) noexcept
{
    return v.index() == alternative_index_v<V, T>;
}

template <Size I, typename V>
    requires(I < std::remove_cv_t<V>::alternatives)
[[nodiscard]] auto* get_if(V* variant)
{
    return detail::Variant_Get_Impl::get_if<I>(variant);
}

template <typename T, typename V>
    requires has_alternative_v<std::remove_cv_t<V>, T>
[[nodiscard]] const_like_t<T, V>* get_if(V* variant) noexcept
{
    return detail::Variant_Get_Impl::get_if<alternative_index_v<std::remove_cvref_t<V>, T>>(
        variant);
}

template <Size I, typename V>
    requires(I < std::remove_cvref_t<V>::alternatives)
[[nodiscard]] decltype(auto) get(V&& variant)
{
    return detail::Variant_Get_Impl::get<I>(std::forward<V>(variant));
}

template <typename T, typename V>
[[nodiscard]] auto get(V&& variant) noexcept
    -> decltype(detail::Variant_Get_Impl::get<alternative_index_v<std::remove_cvref_t<V>, T>>(
        std::forward<V>(variant)))
{
    return detail::Variant_Get_Impl::get<alternative_index_v<std::remove_cvref_t<V>, T>>(
        std::forward<V>(variant));
}

} // namespace bit_manipulation

#endif
