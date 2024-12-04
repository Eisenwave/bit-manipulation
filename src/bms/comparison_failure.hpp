#ifndef BIT_MANIPULATION_BMS_COMPARISON_FAILURE_HPP
#define BIT_MANIPULATION_BMS_COMPARISON_FAILURE_HPP

#include "bms/tokenization/token_type.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

/// @brief Additional information about comparison failures.
/// This may be used e.g. to display the value of the left and right hand side for failed
/// assertions such as `x == 0`, etc.
struct Comparison_Failure {
    Concrete_Value left, right;
    Expression_Type op;
};

static_assert(std::is_trivially_copyable_v<Comparison_Failure>);

} // namespace bit_manipulation::bms

#endif
