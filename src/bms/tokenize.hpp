#ifndef BIT_MANIPULATION_BMS_TOKENIZE_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_HPP

#include <string_view>
#include <vector>

#include "common/result.hpp"

#include "bms/diagnostic_consumer.hpp"
#include "bms/tokenize_error.hpp"

namespace bit_manipulation::bms {

Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out, std::string_view source);

inline bool
tokenize(std::pmr::vector<Token>& out, std::string_view source, Diagnostic_Consumer& diagnostics)
{
    if (auto result = tokenize(out, source)) {
        return true;
    }
    else {
        diagnostics(std::move(result.error()));
        return false;
    }
}

} // namespace bit_manipulation::bms

#endif
