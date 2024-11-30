#ifndef BIT_MANIPULATION_BMS_TOKENIZE_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_HPP

#include <string_view>
#include <vector>

#include "common/result.hpp"

#include "bms/fwd.hpp"
#include "bms/tokenization/tokenize_error.hpp"

namespace bit_manipulation::bms {

Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out, std::string_view source);

bool tokenize(std::pmr::vector<Token>& out,
              std::string_view source,
              Diagnostic_Consumer& diagnostics);

} // namespace bit_manipulation::bms

#endif
