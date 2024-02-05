#ifndef BIT_MANIPULATION_BMS_DEDUCTION_HPP
#define BIT_MANIPULATION_BMS_DEDUCTION_HPP

#include <span>
#include <variant>

namespace bit_manipulation::bms {

using Widths = std::variant<int, std::span<const int>>;

} // namespace bit_manipulation::bms

#endif