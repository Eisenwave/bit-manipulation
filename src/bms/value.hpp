#ifndef BIT_MANIPULATION_BMS_VALUE_HPP
#define BIT_MANIPULATION_BMS_VALUE_HPP

#include <optional>
#include <variant>

#include "assert.hpp"

#include "bms/fwd.hpp"
#include "bms/type.hpp"

namespace bit_manipulation::bms {

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
    static const Concrete_Value Void, True, False;
    [[nodiscard]] static constexpr Concrete_Value Int(Big_Int value) noexcept
    {
        return { Concrete_Type::Int, value };
    }

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

inline constexpr Concrete_Value Concrete_Value::Void { Concrete_Type::Void, 0 };
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
    static const Value Void, True, False;

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

    constexpr bool is_unknown() const noexcept
    {
        return !int_value.has_value();
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

inline constexpr Value Value::Void { Concrete_Type::Void, 0 };
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