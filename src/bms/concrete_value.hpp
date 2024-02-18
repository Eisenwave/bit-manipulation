#ifndef BIT_MANIPULATION_CONCRETE_VALUE_HPP
#define BIT_MANIPULATION_CONCRETE_VALUE_HPP

#include "result.hpp"

#include "bms/concrete_type.hpp"

namespace bit_manipulation::bms {

enum struct Conversion_Type : Default_Underlying {
    /// @brief A numeric conversion which fails if any information is lost.
    lossless_numeric,
    /// @brief A numeric conversion where any non-representable information is discarded.
    truncating_numeric,
};

enum struct Conversion_Error_Code : Default_Underlying {
    not_convertible,
    int_to_uint_range_error,
};

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

    constexpr Result<Concrete_Value, Conversion_Error_Code>
    convert_to(Concrete_Type other, Conversion_Type conversion) const
    {
        if (type == other) {
            return *this;
        }
        if (!type.is_convertible_to(other)) {
            return Conversion_Error_Code::not_convertible;
        }
        if (other.is_uint()) {
            const auto truncated = Big_Uint(int_value) & other.get_mask();
            if (conversion == Conversion_Type::lossless_numeric
                && truncated != Big_Uint(int_value)) {
                return Conversion_Error_Code::int_to_uint_range_error;
            }
            return Concrete_Value { other, Big_Int(truncated) };
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Impossible type conversion requested.");
    }

    constexpr Concrete_Value transform_uint(Big_Uint f(Big_Uint)) const
    {
        BIT_MANIPULATION_ASSERT(type.is_uint());
        const auto mask = Big_Uint(Big_Uint(1) << type.width()) - 1;
        return { type, Big_Int(Big_Uint(f(Big_Uint(int_value))) & mask) };
    }
};

inline constexpr Concrete_Value Concrete_Value::Void { Concrete_Type::Void, 0 };
inline constexpr Concrete_Value Concrete_Value::True { Concrete_Type::Bool, 1 };
inline constexpr Concrete_Value Concrete_Value::False { Concrete_Type::Bool, 0 };

} // namespace bit_manipulation::bms

#endif