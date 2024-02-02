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

struct Concrete_Value {
    Concrete_Type type;
    Big_Int int_value;

public:
    static const Concrete_Value True, False;

    constexpr Concrete_Value(Concrete_Type type, Big_Int value)
        : type(type)
        , int_value(value)
    {
    }

    constexpr Concrete_Value()
        : Concrete_Value(Concrete_Type::Void, 0)
    {
    }

    struct Conversion_Result;

    constexpr Conversion_Result convert_to(Concrete_Type other) const;

    constexpr Concrete_Value transform_uint(Big_Uint f(Big_Uint)) const
    {
        BIT_MANIPULATION_ASSERT(type.type == Type_Type::Uint);
        const auto mask = Big_Uint(Big_Uint(1) << type.width) - 1;
        return { type, Big_Int(Big_Uint(f(Big_Uint(int_value))) & mask) };
    }
};

struct Concrete_Value::Conversion_Result {
    /// @brief The value resulting from the conversion, of type `Uint(N)`.
    Concrete_Value value;
    /// @brief `true` if the conversion was lossy, i.e. if the value couldn't be represented in the
    /// unsigned integer due to limited width.
    bool lossy;
};

inline constexpr Concrete_Value Concrete_Value::True { Concrete_Type::Bool, 1 };
inline constexpr Concrete_Value Concrete_Value::False { Concrete_Type::Bool, 0 };

constexpr auto Concrete_Value::convert_to(Concrete_Type other) const -> Conversion_Result
{
    if (type == other) {
        return { *this, false };
    }
    else if (other.type == Type_Type::Uint) {
        const bool lossy = !other.can_represent(int_value);
        return { Concrete_Value { other, int_value }, lossy };
    }
    BIT_MANIPULATION_ASSERT(false);
}

/// @brief A class representing a possibly value, predominantly used in semantic analysis and
/// constant folding.
/// Unlike `Concrete_Value`, its `int_value` is optional, although its type is always known.
struct Value {
    Concrete_Type type;
    std::optional<Big_Int> int_value;

public:
    static const Value True, False;

    constexpr explicit Value(Concrete_Type type, std::optional<Big_Int> value = {})
        : type(type)
        , int_value(value)
    {
    }

    constexpr Value(Concrete_Value value)
        : type(value.type)
        , int_value(value.int_value)
    {
    }

    explicit constexpr operator bool() const noexcept
    {
        return int_value.has_value();
    }

    template <std::invocable<Big_Int> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Int>, Big_Int>
    constexpr Value and_then(F f) const
    {
        if (int_value) {
            return Value { type, f(*int_value) };
        }
        return *this;
    }

    template <std::invocable<Big_Uint> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Uint>, Big_Uint>
    constexpr Value and_then_uint(F f) const
    {
        if (int_value) {
            const auto mask = Big_Uint(Big_Uint(1) << type.width) - 1;
            return Value { type, Big_Int(Big_Uint(f(Big_Uint(*int_value))) & mask) };
        }
        return *this;
    }

    struct Conversion_Result;

    constexpr Conversion_Result convert_to(Concrete_Type other) const;

    constexpr Concrete_Value concrete_value() const
    {
        return Concrete_Value { type, int_value.value() };
    }
};

struct Value::Conversion_Result {
    /// @brief The value resulting from the conversion, of type `Uint(N)`.
    Value value;
    /// @brief `true` if the conversion was lossy, i.e. if the value couldn't be represented in the
    /// unsigned integer due to limited width.
    bool lossy;
};

inline constexpr Value Value::True { Concrete_Type::Bool, 1 };
inline constexpr Value Value::False { Concrete_Type::Bool, 0 };

constexpr auto Value::convert_to(Concrete_Type other) const -> Conversion_Result
{
    if (type == other) {
        return { *this, false };
    }
    else if (other.type == Type_Type::Uint) {
        if (!int_value) {
            return { Value { other }, false };
        }
        const bool lossy = !other.can_represent(*int_value);
        return { Value { other, *int_value }, lossy };
    }
    BIT_MANIPULATION_ASSERT(false);
}

} // namespace bit_manipulation::bms

#endif