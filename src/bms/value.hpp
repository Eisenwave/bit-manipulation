#ifndef BIT_MANIPULATION_BMS_VALUE_HPP
#define BIT_MANIPULATION_BMS_VALUE_HPP

#include <optional>
#include <variant>

#include "assert.hpp"

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief A class representing a possibly value, predominantly used in semantic analysis and
/// constant folding.
/// Unlike `Concrete_Value`, its `int_value` is optional, although its type is always known.
struct Value {
    Concrete_Type type;
    std::optional<Big_Int> int_value;

public:
    static const Value Void, True, False;
    static constexpr Value Int(Big_Int value) noexcept
    {
        return Value { Concrete_Type::Int, value };
    }

    static constexpr Value unknown_of_type(Concrete_Type type) noexcept
    {
        return Value { type };
    }

private:
    constexpr explicit Value(Concrete_Type type, std::optional<Big_Int> value = {})
        : type(type)
        , int_value(value)
    {
    }

public:
    constexpr Value(Concrete_Value value)
        : type(value.type)
        , int_value(value.int_value)
    {
    }

    explicit constexpr operator bool() const noexcept
    {
        return int_value.has_value();
    }

    constexpr bool is_known() const noexcept
    {
        return int_value.has_value();
    }

    constexpr bool is_unknown() const noexcept
    {
        return !is_known();
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
            const auto mask = Big_Uint(Big_Uint(1) << type.width()) - 1;
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
    else if (other.is_uint()) {
        if (!int_value) {
            return { Value { other }, false };
        }
        const bool lossy = !other.can_represent(*int_value);
        return { Value { other, *int_value }, lossy };
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Impossible conversion requested.");
}

} // namespace bit_manipulation::bms

#endif