#include <ostream>

#include "common/ansi.hpp"
#include "common/code_span_type.hpp"
#include "common/code_string.hpp"

#include "bmd/code_language.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/html/doc_to_html.hpp"

#include "cli/glue.hpp"

namespace bit_manipulation {

#if 0
/// @brief Writes the document with standard stylesheets attached and
/// the usual indent options.
Result<void, bmd::Document_Error> write_html(bmd::HTML_Token_Consumer& out,
                                             const bmd::Parsed_Document& document,
                                             std::pmr::memory_resource* memory)
{
    static constexpr std::string_view stylesheets[] { "/css/code.css", "/css/main.css" };

    constexpr bmd::Document_Options options { .indent_width = 2, //
                                              .stylesheets = stylesheets };

    return bmd::doc_to_html(out, document, options, memory);
}
#endif

} // namespace bit_manipulation
