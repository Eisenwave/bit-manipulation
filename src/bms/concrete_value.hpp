#ifndef BIT_MANIPULATION_CONCRETE_VALUE_HPP
#define BIT_MANIPULATION_CONCRETE_VALUE_HPP

#include "bms/concrete_type.hpp"

namespace bit_manipulation::bms {

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
        BIT_MANIPULATION_ASSERT(type.is_uint());
        const auto mask = Big_Uint(Big_Uint(1) << type.width()) - 1;
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
    else if (other.is_uint()) {
        const bool lossy = !other.can_represent(int_value);
        return { Concrete_Value { other, int_value }, lossy };
    }
    BIT_MANIPULATION_ASSERT(false);
}

} // namespace bit_manipulation::bms

#endif