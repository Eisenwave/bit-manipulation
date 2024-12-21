#ifndef BIT_MANIPULATION_BMS_TOKENIZE_HPP
#define BIT_MANIPULATION_BMS_TOKENIZE_HPP

#include <string_view>
#include <vector>

#include "common/result.hpp"

#include "bms/fwd.hpp"
#include "bms/tokenization/tokenize_error.hpp"

namespace bit_manipulation::bms {

enum struct Tokenize_Mode {
    /// @brief Stop tokenization on the first error.
    single_error,
    /// @brief Continue tokenization after an error.
    /// All diagnostics will be collected through `Diagnostic_Consumer`.
    multi_error
};

Result<void, Tokenize_Error> tokenize(std::pmr::vector<Token>& out, std::string_view source);

bool tokenize(std::pmr::vector<Token>& out,
              std::string_view source,
              Diagnostic_Consumer& diagnostics,
              Tokenize_Mode mode = Tokenize_Mode::single_error);

} // namespace bit_manipulation::bms

#endif
