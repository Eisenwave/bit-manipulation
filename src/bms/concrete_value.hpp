#ifndef BIT_MANIPULATION_CONCRETE_VALUE_HPP
#define BIT_MANIPULATION_CONCRETE_VALUE_HPP

#include <iosfwd>

#include "common/result.hpp"

#include "bms/concrete_type.hpp"
#include "bms/evaluation/evaluation_error.hpp"

namespace bit_manipulation::bms {

enum struct Conversion_Type : Default_Underlying {
    /// @brief A numeric conversion which fails if any information is lost.
    lossless_numeric,
    /// @brief A numeric conversion where any non-representable information is discarded.
    truncating_numeric,
};

struct Concrete_Value {
    Concrete_Type type;
    Big_Int int_value;

public:
    static const Concrete_Value Nothing, Void, True, False;

    [[nodiscard]] static constexpr Concrete_Value Int(Big_Int value) noexcept
    {
        return { Concrete_Type::Int, value };
    }

    [[nodiscard]] constexpr Concrete_Value(const Concrete_Type& type, Big_Int value)
        : type(type)
        , int_value(value)
    {
    }

    [[nodiscard]] constexpr Concrete_Value()
        : Concrete_Value(Concrete_Type::Void, 0)
    {
    }

    [[nodiscard]] friend std::strong_ordering operator<=>(const Concrete_Value&,
                                                          const Concrete_Value&)
        = default;

    [[nodiscard]] constexpr Result<Concrete_Value, Evaluation_Error_Code>
    convert_to(const Concrete_Type& other, Conversion_Type conversion) const
    {
        if (type == other) {
            return *this;
        }
        if (!type.is_convertible_to(other)) {
            return Evaluation_Error_Code::type_error;
        }
        if (other.is_uint()) {
            const auto truncated = Big_Uint(int_value) & other.get_mask();
            if (conversion == Conversion_Type::lossless_numeric
                && truncated != Big_Uint(int_value)) {
                return Evaluation_Error_Code::int_to_uint_range_error;
            }
            return Concrete_Value { other, Big_Int(truncated) };
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Impossible type conversion requested.");
    }

    [[nodiscard]] constexpr Concrete_Value transform_uint(Big_Uint f(Big_Uint)) const
    {
        BIT_MANIPULATION_ASSERT(type.is_uint());
        const auto mask = Big_Uint(Big_Uint(1) << type.width()) - 1;
        return { type, Big_Int(Big_Uint(f(Big_Uint(int_value))) & mask) };
    }
};

inline constexpr Concrete_Value Concrete_Value::Nothing { Concrete_Type::Nothing, 0 };
inline constexpr Concrete_Value Concrete_Value::Void { Concrete_Type::Void, 0 };
inline constexpr Concrete_Value Concrete_Value::True { Concrete_Type::Bool, 1 };
inline constexpr Concrete_Value Concrete_Value::False { Concrete_Type::Bool, 0 };

std::ostream& operator<<(std::ostream&, const Concrete_Value&);

} // namespace bit_manipulation::bms

#endif
