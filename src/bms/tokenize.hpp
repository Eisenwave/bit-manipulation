#ifndef BIT_MANIPULATION_BMS_TOKENIZE_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_HPP

#include <string_view>
#include <vector>

#include "result.hpp"

#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

Result<void, Source_Position> tokenize(std::vector<Token>& out, std::string_view source) noexcept;

} // namespace bit_manipulation::bms

#endif