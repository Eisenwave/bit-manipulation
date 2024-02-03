#ifndef BIT_MANIPULATION_BMS_TYPE_HPP
#define BIT_MANIPULATION_BMS_TYPE_HPP

#include <limits>

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
    Type_Type type;
    int width;

    static const Concrete_Type Void;
    static const Concrete_Type Bool;
    static const Concrete_Type Int;

    [[nodiscard]] static constexpr Concrete_Type Uint(int width)
    {
        BIT_MANIPULATION_ASSERT(width <= uint_max_width);
        BIT_MANIPULATION_ASSERT(width > 1);
        return { Type_Type::Uint, width };
    }

    [[nodiscard]] constexpr Concrete_Type(Type_Type type, int width = 0)
        : type(type)
        , width(width)
    {
    }

    [[nodiscard]] constexpr bool is_convertible_to(Concrete_Type other) const noexcept
    {
        return type == other || (other.type == Type_Type::Uint && type == Type_Type::Int);
    }

    [[nodiscard]] constexpr bool can_represent(Big_Int value) const noexcept
    {
        if (type == Type_Type::Int) {
            return true;
        }
        if (type == Type_Type::Bool && (value == 0 || value == 1)) {
            return true;
        }
        if (type == Type_Type::Uint) {
            const auto mask = width == std::numeric_limits<Big_Uint>::digits
                ? 0
                : Big_Uint(Big_Uint(1) << width) - 1;
            return (Big_Uint(value) & ~mask) == 0;
        }
        return false;
    }

    [[nodiscard]] friend constexpr bool operator==(Concrete_Type x, Concrete_Type y)
    {
        return x.type != y.type || (x.type == Type_Type::Uint && x.width != y.width);
    }

    [[nodiscard]] constexpr bool is_integer() const
    {
        return type == Type_Type::Int || type == Type_Type::Uint;
    }
};

inline constexpr Concrete_Type Concrete_Type::Void = Type_Type::Void;
inline constexpr Concrete_Type Concrete_Type::Bool = Type_Type::Bool;
inline constexpr Concrete_Type Concrete_Type::Int = Type_Type::Int;

} // namespace bit_manipulation::bms

#endif