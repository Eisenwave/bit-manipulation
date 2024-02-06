#ifndef BIT_MANIPULATION_PARSE_NUMBER_HPP
#define BIT_MANIPULATION_PARSE_NUMBER_HPP

#include <concepts>
#include <optional>
#include <string_view>

#include "config.hpp"

namespace bit_manipulation::bms {

std::optional<Big_Uint> parse_uinteger_literal(std::string_view str) noexcept;

std::optional<Big_Int> parse_integer_literal(std::string_view str) noexcept;

} // namespace bit_manipulation::bms

#endif