#ifndef BIT_MANIPULATION_BMS_TYPE_TYPE_HPP
#define BIT_MANIPULATION_BMS_TYPE_TYPE_HPP

#include <string_view>

#include "common/assert.hpp"
#include "common/config.hpp"

namespace bit_manipulation::bms {

/// @brief A type in the BMS language, without specified width.
enum struct Type_Type : Default_Underlying {
    /// @brief A type with no values.
    Nothing,
    /// @brief A type exactly one possible value, `Void`.
    Void,
    /// @brief A boolean type: true or false.
    Bool,
    /// @brief An infinite precision integer.
    Int,
    /// @brief An arbitrary precision unsigned integer.
    Uint,
};

[[nodiscard]] constexpr std::string_view type_type_name(Type_Type type)
{
    using enum Type_Type;
    switch (type) {
        BIT_MANIPULATION_ENUM_STRING_CASE(Nothing);
        BIT_MANIPULATION_ENUM_STRING_CASE(Void);
        BIT_MANIPULATION_ENUM_STRING_CASE(Bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(Int);
        BIT_MANIPULATION_ENUM_STRING_CASE(Uint);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type type.");
}

} // namespace bit_manipulation::bms

#endif
