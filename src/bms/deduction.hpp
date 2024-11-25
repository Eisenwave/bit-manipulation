#ifndef BIT_MANIPULATION_BMS_DEDUCTION_HPP
#define BIT_MANIPULATION_BMS_DEDUCTION_HPP

#include "common/variant.hpp"

#include <span>

namespace bit_manipulation::bms {

/// @brief A bit-width to be used for bit-generic types.
/// A width can either be a single `int`, which is a convenient way of specifying the same width
/// everywhere, or it can be a concrete set of widths.
/// The latter is always used in implicit instantiations; the former may be used for testing,
/// or for auto-instantiating everything with a pre-set width for the purpose of codegen.
using Widths = Variant<int, std::span<const int>>;

inline int get_width(const Widths& w, Size i)
{
    if (const auto* single_width = get_if<int>(&w)) {
        return *single_width;
    }
    else {
        auto& span = get<std::span<const int>>(w);
        BIT_MANIPULATION_ASSERT(i < span.size());
        return span[i];
    }
}

} // namespace bit_manipulation::bms

#endif
