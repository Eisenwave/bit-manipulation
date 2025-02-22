#if 0
#ifndef BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP
#define BIT_MANIPULATION_BMD_DOC_TO_HTML_HPP

#include <span>
#include <string_view>

#include "common/result.hpp"
#include "common/source_position.hpp"

#include "bmd/fwd.hpp"
#include "bmd/html/document_error.hpp"

namespace bit_manipulation::bmd {

struct Document_Options {
    Size indent_width;
    std::span<const std::string_view> stylesheets {};
};

/// @brief Converts the given parsed document to an HTML string.
/// @param out the string to write to; the contents will be entirely replaced
/// @param document the document to convert
/// @param options the options
/// @param memory a source of temporary memory for subroutines such as BMS tokenization
[[nodiscard]] Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out,
                                                       const Parsed_Document& document,
                                                       Document_Options options,
                                                       std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
#endif
