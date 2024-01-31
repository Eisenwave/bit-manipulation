#ifndef BIT_MANIPULATION_BMS_VALUE_HPP
#define BIT_MANIPULATION_BMS_VALUE_HPP

#include <optional>
#include <variant>

#include "assert.hpp"

#include "bms/fwd.hpp"

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
    Uint
};

struct Concrete_Type {
    Type_Type type;
    int width;

    static const Concrete_Type Void;
    static const Concrete_Type Bool;
    static const Concrete_Type Int;

    static constexpr Concrete_Type Uint(int width)
    {
        BIT_MANIPULATION_ASSERT(width <= uint_max_width);
        BIT_MANIPULATION_ASSERT(width > 1);
        return { Type_Type::Uint, width };
    }

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

inline constexpr Concrete_Type Concrete_Type::Void = Type_Type::Void;
inline constexpr Concrete_Type Concrete_Type::Bool = Type_Type::Bool;
inline constexpr Concrete_Type Concrete_Type::Int = Type_Type::Int;

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
    std::optional<Big_Int> value;

public:
    constexpr explicit Value(Concrete_Type type)
        : type(type)
    {
    }

    constexpr Value(Concrete_Type type, Big_Int value)
        : type(type)
        , value(value)
    {
    }

    explicit constexpr operator bool() const noexcept
    {
        return value.has_value();
    }

    template <std::invocable<Big_Int> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Int>, Big_Int>
    constexpr Value and_then(F f) const
    {
        if (value) {
            return { type, f(*value) };
        }
        return *this;
    }

    template <std::invocable<Big_Uint> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Uint>, Big_Uint>
    constexpr Value and_then_uint(F f) const
    {
        if (value) {
            const auto mask = Big_Uint(Big_Uint(1) << type.width) - 1;
            return { type, Big_Int(Big_Uint(f(Big_Uint(*value))) & mask) };
        }
        return *this;
    }

    struct To_Uint_Result;

    /// @brief Converts the current value to an unsigned integer of the given width.
    /// @param width the width, which must be valid for calling `Concrete_Type::Uint`.
    constexpr To_Uint_Result to_uint(int width) const;
};

struct Value::To_Uint_Result {
    /// @brief The value resulting from the conversion, of type `Uint(N)`.
    Value value;
    /// @brief `true` if the conversion was lossy, i.e. if the value couldn't be represented in the
    /// unsigned integer due to limited width.
    bool lossy;
};

constexpr auto Value::to_uint(int width) const -> To_Uint_Result
{
    const auto type = Concrete_Type::Uint(width);
    if (!value) {
        return { .value = Value { type }, .lossy = false };
    }
    auto result = Big_Uint(*value);
    const auto mask
        = width == std::numeric_limits<Big_Uint>::digits ? 0 : Big_Uint(Big_Uint(1) << width) - 1;
    const bool lossy = (result & ~mask) != 0;
    return { Value { type, Big_Int(result) }, lossy };
}

} // namespace bit_manipulation::bms

#endif