#ifndef BIT_MANIPULATION_BMS_VALUE_HPP
#define BIT_MANIPULATION_BMS_VALUE_HPP

#include "common/assert.hpp"

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief A class representing a possibly unknown value, predominantly used in semantic analysis
/// and constant folding. Unlike `Concrete_Value`, its `int_value` is optional, although its type is
/// always known.
struct Value {
private:
    Concrete_Type m_type;
    Big_Int m_int_value;
    bool m_known;

public:
    /// @brief Known value of type `Nothing`.
    static const Value Nothing;
    /// @brief Known value of type `Void`.
    static const Value Void;
    /// @brief Known `true` constant.
    static const Value True;
    /// @brief Known `false` constant.
    static const Value False;

    /// @brief Returns `Value(Concrete_Value(Concrete_Type::Int, value))`.
    [[nodiscard]] static constexpr Value Int(Big_Int value) noexcept
    {
        return Value { Concrete_Type::Int, value };
    }

    /// @brief Returns an unknown value of the given type.
    [[nodiscard]] static constexpr Value unknown_of_type(Concrete_Type type) noexcept
    {
        return Value { type };
    }

private:
    [[nodiscard]] constexpr explicit Value(Concrete_Type type, Big_Int value)
        : m_type(type)
        , m_int_value(value)
        , m_known(true)
    {
    }

    [[nodiscard]] constexpr explicit Value(Concrete_Type type)
        : m_type(type)
        , m_int_value(0)
        , m_known(false)
    {
    }

public:
    /// @brief Constructs a known (concrete) `Value` from the given `value`.
    [[nodiscard]] constexpr Value(Concrete_Value value)
        : m_type(value.get_type())
        , m_int_value(value.as_int())
        , m_known(true)
    {
    }

    /// @brief Equivalent to `is_known()`.
    [[nodiscard]] explicit constexpr operator bool() const
    {
        return m_known;
    }

    [[nodiscard]] constexpr bool is_known() const
    {
        return m_known;
    }

    [[nodiscard]] constexpr bool is_unknown() const
    {
        return !is_known();
    }

    [[nodiscard]] const Concrete_Type& get_type() const
    {
        return m_type;
    }

    /// @brief If this value is known, applies the given transformation function `f` to that value
    /// and constructs a new `Value`.
    /// @tparam F a unary function `Big_int(Big_Int)`
    /// @param f the transformation function
    /// @return a possibly transformed copy of the this value
    template <std::invocable<Big_Int> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Int>, Big_Int>
    [[nodiscard]] constexpr Value and_then(F f) const
    {
        if (is_known()) {
            return Value { m_type, f(m_int_value) };
        }
        return *this;
    }

    /// @brief Like `and_then`, but uses a `Big_Int(Big_Int)` transformation function.
    template <std::invocable<Big_Uint> F>
        requires std::convertible_to<std::invoke_result_t<F, Big_Uint>, Big_Uint>
    [[nodiscard]] constexpr Value and_then_uint(F f) const
    {
        if (is_known()) {
            const auto mask = Big_Uint(Big_Uint(1) << m_type.width()) - 1;
            return Value { m_type, Big_Int(Big_Uint(f(Big_Uint(m_int_value))) & mask) };
        }
        return *this;
    }

    /// @brief Returns this value, converted to the given target type.
    /// If this value is unknown, returns an unknown value of the target type.
    /// @param other the type to convert to
    /// @param conversion the type of conversion
    /// @return the converted value or error code
    [[nodiscard]] constexpr Result<Value, Evaluation_Error_Code>
    convert_to(const Concrete_Type& other, Conversion_Type conversion) const
    {
        if (!m_type.is_convertible_to(other)) {
            return Evaluation_Error_Code::type_error;
        }
        if (is_unknown()) {
            return Value::unknown_of_type(other);
        }
        auto result = concrete_value().convert_to(other, conversion);
        if (!result) {
            return result.error();
        }
        return Value { *result };
    }

    /// @brief Returns the concrete value that this value represents.
    /// @throws Throws if `is_unknown()`.
    /// @return A `Concrete_Value`.
    [[nodiscard]] constexpr Concrete_Value concrete_value() const
    {
        BIT_MANIPULATION_ASSERT(is_known());
        return Concrete_Value { m_type, m_int_value };
    }

    /// @brief Returns the concrete boolean value that this value represents.
    /// @throws Throws if `is_unknown() || type != Concrete_Type::Bool`.
    /// @return A `bool`.
    [[nodiscard]] constexpr bool as_bool() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type == Concrete_Type::Bool);
        return bool(m_int_value);
    }

    /// @brief Returns the concrete integer value that this value represents.
    /// @throws Throws if `is_unknown() || type != Concrete_Type::Int`.
    /// @return A `Big_Int`.
    [[nodiscard]] constexpr Big_Int as_int() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type == Concrete_Type::Int);
        return m_int_value;
    }

    /// @brief Returns the concrete unsigned integer value that this value represents.
    /// @throws Throws if `is_unknown() || !type.is_uint()`.
    /// @return A `Big_Int`.
    [[nodiscard]] constexpr Big_Uint as_uint() const
    {
        BIT_MANIPULATION_ASSERT(is_known() && m_type.is_uint());
        return static_cast<Big_Uint>(m_int_value);
    }
};

inline constexpr Value Value::Nothing { Concrete_Type::Nothing, 0 };
inline constexpr Value Value::Void { Concrete_Type::Void, 0 };
inline constexpr Value Value::True { Concrete_Type::Bool, 1 };
inline constexpr Value Value::False { Concrete_Type::Bool, 0 };

static_assert(std::is_trivially_copyable_v<Value>);

} // namespace bit_manipulation::bms

#endif
