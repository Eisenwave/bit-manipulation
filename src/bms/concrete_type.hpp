#ifndef BIT_MANIPULATION_CONCRETE_TYPE_HPP
#define BIT_MANIPULATION_CONCRETE_TYPE_HPP

#include "assert.hpp"
#include "config.hpp"

namespace bit_manipulation::bms {

/// @brief A type in the BMS language.
enum struct Type_Type {
    /// @brief A type with no values.
    Void,
    /// @brief A boolean type: true or false.
    Bool,
    /// @brief An infinite precision integer.
    Int,
    /// @brief An arbitrary precision unsigned integer.
    Uint,
};

struct Concrete_Type {
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

    [[nodiscard]] constexpr explicit Concrete_Type(Type_Type type, int width)
        : m_type(type)
        , m_width(width)
    {
    }

public:
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

    [[nodiscard]] constexpr bool can_represent(Big_Int value) const noexcept
    {
        if (m_type == Type_Type::Int) {
            return true;
        }
        if (m_type == Type_Type::Bool && (value == 0 || value == 1)) {
            return true;
        }
        if (m_type == Type_Type::Uint) {
            const auto mask = m_width == std::numeric_limits<Big_Uint>::digits
                ? 0
                : Big_Uint(Big_Uint(1) << m_width) - 1;
            return (Big_Uint(value) & ~mask) == 0;
        }
        return false;
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

inline constexpr Concrete_Type Concrete_Type::Void { Type_Type::Void, 0 };
inline constexpr Concrete_Type Concrete_Type::Bool { Type_Type::Bool, 0 };
inline constexpr Concrete_Type Concrete_Type::Int { Type_Type::Int, 0 };

} // namespace bit_manipulation::bms

#endif