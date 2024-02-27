#ifndef BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP

#include <string_view>

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

enum struct Bms_To_Html_Status {
    /// @brief The given code or codeblock could be parsed in some way, allowing full conversion.
    ok,
    /// @brief Only token-based conversion was possible due to an issue with parsing.
    token_based,
    /// @brief Tokenization failed, preventing conversion beyond plain text.
    error,
};

Bms_To_Html_Status bms_inline_code_to_html(HTML_Token_Consumer& out, std::string_view code);

} // namespace bit_manipulation::bmd

#endif
