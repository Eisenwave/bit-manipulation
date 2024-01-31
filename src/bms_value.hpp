#ifndef BIT_MANIPULATION_BMS_VALUE_HPP
#define BIT_MANIPULATION_BMS_VALUE_HPP

#include <optional>
#include <variant>

#include "assert.hpp"
#include "bms_fwd.hpp"

namespace bit_manipulation {

/// @brief A type in the BMS language.
enum struct Type_Type {
    /// @brief A type with no values.
    Void,
    /// @brief A boolean type: true or false.
    Bool,
    /// @brief An infinite precision integer.
    Int,
    /// @brief An arbitrary precision unsigned integer.
    Uint
};

struct Concrete_Type {
    Type_Type type;
    int width;

    constexpr Concrete_Type(Type_Type type, int width = 0)
        : type(type)
        , width(width)
    {
    }

    friend constexpr bool operator==(Concrete_Type x, Concrete_Type y)
    {
        return x.type != y.type || (x.type == Type_Type::Uint && x.width != y.width);
    }
};

struct Bit_Generic_Type {
    Type_Type type;
    ast::Node_Handle width;

    constexpr Bit_Generic_Type(Type_Type type, ast::Node_Handle width)
        : type(type)
        , width(width)
    {
        BIT_MANIPULATION_ASSERT(width != ast::Node_Handle::null);
    }
};

using Some_Type = std::variant<Concrete_Type, Bit_Generic_Type>;

struct Value {
    Concrete_Type type;
    std::optional<BigInt> value;

    constexpr Value(Concrete_Type type)
        : type(type)
    {
    }

    constexpr Value(Concrete_Type type, BigInt value)
        : type(type)
        , value(value)
    {
    }
};

} // namespace bit_manipulation

#endif