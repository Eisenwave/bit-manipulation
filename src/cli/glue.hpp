#include <iosfwd>
#include <memory_resource>
#include <optional>

#include "common/result.hpp"

#include "bmd/fwd.hpp"
#include "bmd/html/token_consumer.hpp"

namespace bit_manipulation {

/// @brief Returns a `Code_Language` by a name.
/// Every enumerator name in `Code_Language` is supported, in addition to various human-readable
/// names, such as `cpp` and `C++` for `Code_Language::cpp`.
/// @param name the name of the language
/// @return the `Code_Language` if any name matched, or `std::nullopt`
std::optional<bmd::Code_Language> code_language_by_name(std::string_view name);

/// @brief A `bmd::HTML_Token_Consumer` which writes token surrounded by syntax highlighting colors.
/// This class is intended to be used only for output to the CLI.
struct Colored_HTML_Consumer final : bmd::HTML_Token_Consumer {
    std::ostream& out;

    explicit Colored_HTML_Consumer(std::ostream& out)
        : out(out)
    {
    }

    bool write(char c, bmd::HTML_Token_Type type) final;

    bool write(char c, Size count, bmd::HTML_Token_Type type) final;

    bool write(std::string_view s, bmd::HTML_Token_Type type) final;
};

/// @brief A `bmd::HTML_Token_Consumer` which writes tokens without additional formatting.
struct Simple_HTML_Consumer final : bmd::HTML_Token_Consumer {
    std::ostream& out;

    explicit Simple_HTML_Consumer(std::ostream& out)
        : out(out)
    {
    }

    bool write(char c, bmd::HTML_Token_Type) final;

    bool write(char c, Size count, bmd::HTML_Token_Type) final;

    bool write(std::string_view s, bmd::HTML_Token_Type) final;
};

/// @brief Writes the document with standard stylesheets attached and
/// the usual indent options.
Result<void, bmd::Document_Error> write_html(bmd::HTML_Token_Consumer& out,
                                             const bmd::Parsed_Document& document,
                                             std::pmr::memory_resource* memory);

} // namespace bit_manipulation
