#ifndef BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP

#include <memory_resource>
#include <string_view>
#include <variant>

#include "common/result.hpp"

#include "bms/parse_error.hpp"
#include "bms/tokenize_error.hpp"

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

using Bms_Error_Variant = std::variant<bms::Tokenize_Error, bms::Parse_Error>;

struct Bms_Error : Bms_Error_Variant {
    using Bms_Error_Variant::variant;
};

/// @brief Converts the given inline code snippet to HTML.
/// If possible, the code snippet is tokenized and parsed.
/// Otherwise if possible, the code snipped is only tokenized.
/// Otherwise, the snippet is converted into teletype text.
/// @param out the html writer to the converted parts to
/// @param code the inline code
/// @param memory the temporary memory used for tokenization and parsing
/// @return Nothing, or `Bms_Error` if parsing or even tokenization fails.
Result<void, Bms_Error>
bms_inline_code_to_html(HTML_Writer& out, std::string_view code, std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
