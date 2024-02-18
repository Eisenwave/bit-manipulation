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
private:
    Concrete_Type m_type;
    std::optional<Big_Int> m_int_value;

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
        : m_type(type)
        , m_int_value(value)
    {
    }

public:
    constexpr Value(Concrete_Value value)
        : m_type(value.type)
        , m_int_value(value.int_value)
    {
    }

    explicit constexpr operator bool() const
    {
        return m_int_value.has_value();
    }

    constexpr bool is_known() const
    {
        return m_int_value.has_value();
    }

    constexpr bool is_unknown() const
    {
        return !is_known();
    }

    [[nodiscard]] Concrete_Type get_type() const
    {
        return m_type;
    }

    template <std::invocable<Big_Int> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Int>, Big_Int>
    constexpr Value and_then(F f) const
    {
        if (m_int_value) {
            return Value { m_type, f(*m_int_value) };
        }
        return *this;
    }

    template <std::invocable<Big_Uint> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Uint>, Big_Uint>
    constexpr Value and_then_uint(F f) const
    {
        if (m_int_value) {
            const auto mask = Big_Uint(Big_Uint(1) << m_type.width()) - 1;
            return Value { m_type, Big_Int(Big_Uint(f(Big_Uint(*m_int_value))) & mask) };
        }
        return *this;
    }

    struct Conversion_Result;

    constexpr Conversion_Result convert_to(Concrete_Type other) const;

    /// @brief Returns the concrete value that this value represents.
    /// @throws Throws if `is_unknown()`.
    /// @return A `Concrete_Value`.
    constexpr Concrete_Value concrete_value() const
    {
        BIT_MANIPULATION_ASSERT(is_known());
        return Concrete_Value { m_type, *m_int_value };
    }

    /// @brief Returns the concrete boolean value that this value represents.
    /// @throws Throws if `is_unknown() || type != Concrete_Type::Bool`.
    /// @return A `bool`.
    constexpr bool as_bool() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type == Concrete_Type::Bool);
        return bool(*m_int_value);
    }

    /// @brief Returns the concrete integer value that this value represents.
    /// @throws Throws if `is_unknown() || type != Concrete_Type::Int`.
    /// @return A `Big_Int`.
    constexpr Big_Int as_int() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type == Concrete_Type::Int);
        return *m_int_value;
    }

    /// @brief Returns the concrete unsigned integer value that this value represents.
    /// @throws Throws if `is_unknown() || !type.is_uint()`.
    /// @return A `Big_Int`.
    constexpr Big_Uint as_uint() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type.is_uint());
        return static_cast<Big_Uint>(*m_int_value);
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
    if (m_type == other) {
        return { *this, false };
    }
    else if (other.is_uint()) {
        if (!m_int_value) {
            return { Value { other }, false };
        }
        const bool lossy = !other.can_represent(*m_int_value);
        return { Value { other, *m_int_value }, lossy };
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Impossible conversion requested.");
}

} // namespace bit_manipulation::bms

#endif