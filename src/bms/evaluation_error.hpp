#ifndef BIT_MANIPULATION_EVALUATION_ERROR_HPP
#define BIT_MANIPULATION_EVALUATION_ERROR_HPP

#include "common/assert.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error_Code : Default_Underlying {
    /// @brief Type-checking of the evaluation failed.
    type_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
    /// @brief An assertion failed.
    assertion_fail,
    /// @brief Conversion from Int to Uint would lose information.
    int_to_uint_range_error
};

constexpr std::string_view evaluation_error_code_name(Evaluation_Error_Code code)
{
    switch (code) {
        using enum Evaluation_Error_Code;
        BIT_MANIPULATION_ENUM_STRING_CASE(type_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(division_by_zero);
        BIT_MANIPULATION_ENUM_STRING_CASE(shift_too_much);
        BIT_MANIPULATION_ENUM_STRING_CASE(assertion_fail);
        BIT_MANIPULATION_ENUM_STRING_CASE(int_to_uint_range_error);
    };
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

} // namespace bit_manipulation::bms

#endif
