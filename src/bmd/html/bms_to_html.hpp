#ifndef BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_BMS_TO_HTML_HPP

#include <memory_resource>
#include <string_view>

#include "common/function_ref.hpp"
#include "common/fwd.hpp"
#include "common/result.hpp"
#include "common/variant.hpp"

#include "bms/parsing/parse_error.hpp"
#include "bms/tokenization/tokenize_error.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

/// @brief Converts the given inline code snippet to HTML.
/// This function uses recoverable tokenization because it is largely intended to be used for
/// syntax highlighting.
/// @param out the html writer to the converted parts to
/// @param code the inline code
/// @param memory the temporary memory used for tokenization and parsing
/// @param on_error optional error callback
/// @return `true` if tokenization succeeded with no errors.
bool bms_inline_code_to_html(HTML_Writer& out,
                             std::string_view code,
                             std::pmr::memory_resource* memory,
                             Function_Ref<void(bms::Tokenize_Error&&)> on_error = {});

/// @brief Convenience function which creates an ad-hoc `HTML_Writer` and calls the other overload.
/// Note that formatting settings are irrelevant anyway, since all HTML output is pre-formatted.
bool bms_inline_code_to_html(Code_String& out,
                             std::string_view code,
                             std::pmr::memory_resource* memory,
                             Function_Ref<void(bms::Tokenize_Error&&)> on_error = {});

} // namespace bit_manipulation::bmd

#endif
