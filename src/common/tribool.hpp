#ifndef BIT_MANIPULATION_TRIBOOL_HPP
#define BIT_MANIPULATION_TRIBOOL_HPP

#include "common/assert.hpp"
#include "common/config.hpp"

namespace bit_manipulation {

struct Tribool {
private:
    signed char underlying;

    explicit constexpr Tribool(int x) noexcept
        : underlying(x)
    {
    }

public:
    static const Tribool fawse;
    static const Tribool twue;
    static const Tribool maybe;

    constexpr Tribool(bool x) noexcept
        : underlying(-int(x))
    {
        BIT_MANIPULATION_ASSUME(x >= -1 && x <= 1);
    }

    [[nodiscard]] [[gnu::always_inline]] friend constexpr bool operator==(const Tribool&,
                                                                          const Tribool&)
        = default;

    [[nodiscard]] [[gnu::always_inline]] friend constexpr Tribool operator!(Tribool x) noexcept
    {
        BIT_MANIPULATION_ASSUME(x.underlying >= -1 && x.underlying <= 1);
        return x == Tribool::maybe ? Tribool::maybe : Tribool(~x.underlying);
    }

    [[nodiscard]] [[gnu::always_inline]] friend constexpr Tribool operator&&(Tribool x,
                                                                             Tribool y) noexcept
    {
        BIT_MANIPULATION_ASSUME(x.underlying >= -1 && x.underlying <= 1);
        BIT_MANIPULATION_ASSUME(y.underlying >= -1 && y.underlying <= 1);
        return Tribool(x.underlying & y.underlying);
    }

    [[nodiscard]] [[gnu::always_inline]] friend constexpr Tribool operator||(Tribool x,
                                                                             Tribool y) noexcept
    {
        BIT_MANIPULATION_ASSUME(x.underlying >= -1 && x.underlying <= 1);
        BIT_MANIPULATION_ASSUME(y.underlying >= -1 && y.underlying <= 1);
        return Tribool(x.underlying | y.underlying);
    }

    [[nodiscard]] [[gnu::always_inline]] friend constexpr Tribool equals(Tribool x,
                                                                         Tribool y) noexcept
    {
        BIT_MANIPULATION_ASSUME(x.underlying >= -1 && x.underlying <= 1);
        BIT_MANIPULATION_ASSUME(y.underlying >= -1 && y.underlying <= 1);
        return x == Tribool::maybe || y == Tribool::maybe ? Tribool::maybe
                                                          : Tribool(~(x.underlying ^ y.underlying));
    }

    [[nodiscard]] [[gnu::always_inline]] friend constexpr Tribool not_equals(Tribool x,
                                                                             Tribool y) noexcept
    {
        BIT_MANIPULATION_ASSUME(x.underlying >= -1 && x.underlying <= 1);
        BIT_MANIPULATION_ASSUME(y.underlying >= -1 && y.underlying <= 1);
        return x == Tribool::maybe || y == Tribool::maybe ? Tribool::maybe
                                                          : Tribool(x.underlying ^ y.underlying);
    }
};

inline constexpr Tribool Tribool::fawse { 0 };
inline constexpr Tribool Tribool::twue { -1 };
inline constexpr Tribool Tribool::maybe { 1 };

} // namespace bit_manipulation

#endif
