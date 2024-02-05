#ifndef BIT_MANIPULATION_BMS_DEDUCTION_HPP
#define BIT_MANIPULATION_BMS_DEDUCTION_HPP

#include <span>
#include <variant>

namespace bit_manipulation::bms {

using Widths = std::variant<int, std::span<const int>>;

constexpr int get_width(const Widths& w, Size i)
{
    if (const auto* single_width = std::get_if<int>(&w)) {
        return *single_width;
    }
    else {
        auto& span = std::get<std::span<const int>>(w);
        BIT_MANIPULATION_ASSERT(i < span.size());
        return span[i];
    }
}

} // namespace bit_manipulation::bms

#endif