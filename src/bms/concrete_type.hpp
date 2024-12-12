#ifndef BIT_MANIPULATION_CONCRETE_TYPE_HPP
#define BIT_MANIPULATION_CONCRETE_TYPE_HPP

#include <iosfwd>
#include <limits>
#include <string_view>

#include "common/assert.hpp"
#include "common/config.hpp"

#include "bms/type_type.hpp"

namespace bit_manipulation::bms {

/// @brief A type in the BMS language, with specified width in the case of `Uint`.
struct Concrete_Type {
    static const Concrete_Type Nothing;
    static const Concrete_Type Void;
    static const Concrete_Type Bool;
    static const Concrete_Type Int;

    /// @brief Returns a `Uint` type with the given width.
    /// @param width an integer in range `[1, uint_max_width]`
    /// @return The `Uint(width)` type.
    [[nodiscard]] static constexpr Concrete_Type Uint(int width)
    {
        BIT_MANIPULATION_ASSERT(width <= uint_max_width);
        BIT_MANIPULATION_ASSERT(width > 1);
        return Concrete_Type { Type_Type::Uint, width };
    }

private:
    Type_Type m_type;
    int m_width;

public:
    [[nodiscard]] constexpr explicit Concrete_Type(Type_Type type, int width = 0)
        : m_type(type)
        , m_width(width)
    {
        BIT_MANIPULATION_ASSERT(type == Type_Type::Uint || width == 0);
        BIT_MANIPULATION_ASSERT(type != Type_Type::Uint || (width > 0 && width < uint_max_width));
    }

    [[nodiscard]] friend constexpr bool operator<=>(Concrete_Type, Concrete_Type) = default;

    /// @brief Returns the type of this type.
    /// @return The type-type.
    [[nodiscard]] constexpr Type_Type type() const noexcept
    {
        return m_type;
    }

    /// @brief Returns the width of this type, or zero if it has none.
    /// @return `0` if this type is not a `Uint`, otherwise the width `N` of the `Uint(N)` type.
    [[nodiscard]] constexpr int width() const noexcept
    {
        return m_width;
    }

    [[nodiscard]] constexpr bool is_convertible_to(Concrete_Type other) const noexcept
    {
        return *this == other || (other.m_type == Type_Type::Uint && m_type == Type_Type::Int);
    }

    /// @brief Returns a mask which covers the bits of the value representation of this type.
    /// For example, if this type is is `Uint(4)`, this function returns `0xf`.
    /// If this type is `Int`, returns a mask where all bits are set.
    /// If this type is `Bool`, returns `1`.
    /// If this type is `Void`, returns `0`.
    /// @return A mask which covers the value representation of this type.
    [[nodiscard]] constexpr Big_Uint get_mask() const
    {
        switch (m_type) {
        case Type_Type::Nothing:
        case Type_Type::Void: return 0;
        case Type_Type::Bool: return 1;
        case Type_Type::Int: return Big_Uint(-1);
        case Type_Type::Uint:
            return m_width == std::numeric_limits<Big_Uint>::digits
                ? 0
                : Big_Uint(Big_Uint(1) << m_width) - 1;
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown type.");
    }

    // TODO: this is somewhat misleading for Uint types, since they cannot actually represent
    //       negatives
    [[nodiscard]] constexpr bool can_represent(Big_Int value) const noexcept
    {
        return m_type != Type_Type::Nothing && m_type != Type_Type::Void
            && (Big_Uint(value) & ~get_mask()) == 0;
    }

    /// @brief Returns `true` if this type is an `Int` or a `Uint` of any width.
    [[nodiscard]] constexpr bool is_integer() const
    {
        return m_type == Type_Type::Int || m_type == Type_Type::Uint;
    }

    /// @brief Returns `true` if this type is a `Uint` of any width.
    [[nodiscard]] constexpr bool is_uint() const
    {
        return m_type == Type_Type::Uint;
    }
};

inline constexpr Concrete_Type Concrete_Type::Nothing { Type_Type::Nothing, 0 };
inline constexpr Concrete_Type Concrete_Type::Void { Type_Type::Void, 0 };
inline constexpr Concrete_Type Concrete_Type::Bool { Type_Type::Bool, 0 };
inline constexpr Concrete_Type Concrete_Type::Int { Type_Type::Int, 0 };

std::ostream& operator<<(std::ostream&, const Concrete_Type&);

} // namespace bit_manipulation::bms

#endif
