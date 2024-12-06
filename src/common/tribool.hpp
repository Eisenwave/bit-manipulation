#ifndef BIT_MANIPULATION_TRIBOOL_HPP
#define BIT_MANIPULATION_TRIBOOL_HPP

#include "common/config.hpp"

namespace bit_manipulation {

enum struct Tribool : signed char { fawse = 0, twue = -1, maybe = 1 };

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool to_tribool(bool x) noexcept
{
    return Tribool(-static_cast<signed char>(x));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator!(Tribool x) noexcept
{
    return x == Tribool::maybe ? Tribool::maybe : Tribool(~static_cast<signed char>(x));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator&&(Tribool x, Tribool y) noexcept
{
    return Tribool(static_cast<signed char>(x) & static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator&&(Tribool x, bool y) noexcept
{
    return Tribool(static_cast<signed char>(x) & -static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator&&(bool x, Tribool y) noexcept
{
    return y && x;
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator||(Tribool x, Tribool y) noexcept
{
    return Tribool(static_cast<signed char>(x) | static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator||(Tribool x, bool y) noexcept
{
    return Tribool(static_cast<signed char>(x) | -static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool operator||(bool x, Tribool y) noexcept
{
    return y && x;
}

[[nodiscard]] [[gnu::always_inline]] constexpr bool operator==(Tribool x, bool y) noexcept
{
    return static_cast<signed char>(x) == -static_cast<signed char>(y);
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool equals(Tribool x, Tribool y) noexcept
{
    return x == Tribool::maybe || y == Tribool::maybe
        ? Tribool::maybe
        : Tribool(~(static_cast<signed char>(x) ^ static_cast<signed char>(y)));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool equals(Tribool x, bool y) noexcept
{
    return x == Tribool::maybe
        ? Tribool::maybe
        : Tribool(~(static_cast<signed char>(x) ^ static_cast<signed char>(y)));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool equals(bool x, Tribool y) noexcept
{
    return equals(y, x);
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool not_equals(Tribool x, Tribool y) noexcept
{
    return x == Tribool::maybe || y == Tribool::maybe
        ? Tribool::maybe
        : Tribool(static_cast<signed char>(x) ^ static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool not_equals(Tribool x, bool y) noexcept
{
    return x == Tribool::maybe ? Tribool::maybe
                               : Tribool(static_cast<signed char>(x) ^ static_cast<signed char>(y));
}

[[nodiscard]] [[gnu::always_inline]] constexpr Tribool not_equals(bool x, Tribool y) noexcept
{
    return not_equals(y, x);
}

} // namespace bit_manipulation

#endif
