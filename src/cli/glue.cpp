#include <ostream>

#include "common/ansi.hpp"

#include "bmd/codegen/code_language.hpp"
#include "bmd/codegen/code_span_type.hpp"
#include "bmd/codegen/code_string.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/html/doc_to_html.hpp"
#include "bmd/html/html_writer.hpp"

#include "cli/glue.hpp"

namespace bit_manipulation {

namespace {

std::string_view highlight_color_of(bmd::HTML_Token_Type type)
{
    using enum bmd::HTML_Token_Type;
    switch (type) {
    case whitespace:
    case inner_text: return ansi::reset;

    case preamble:
    case comment: return ansi::h_black;

    case tag_identifier: return ansi::h_blue;

    case tag_bracket:
    case attribute_equal:
    case attribute_comma: return ansi::black;

    case attribute_key: return ansi::h_cyan;

    case attribute_quote:
    case attribute_value: return ansi::h_green;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown HTML tag type.");
}

std::string_view highlight_color_of(bmd::Code_Span_Type type)
{
    using enum bmd::Code_Span_Type;
    switch (type) {
    case identifier: return ansi::h_white;
    case type_name: return ansi::h_blue;

    case number: return ansi::h_cyan;

    case comment:
    case operation: return ansi::h_black;

    case bracket:
    case punctuation: return ansi::black;

    case keyword:
    case boolean_literal: return ansi::h_magenta;

    case error: return ansi::h_red;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown code span type.");
}

} // namespace

std::optional<bmd::Code_Language> code_language_by_name(std::string_view name)
{
    using enum bmd::Code_Language;
    // clang-format off
    static constexpr struct {
        std::string_view name;
        bmd::Code_Language language;
    } languages_by_name[] = {
        { "bms", bms },
        { "BMS", bms },
        { "c", c },
        { "C", c },
        { "c++", cpp },
        { "C++", cpp },
        { "cpp", cpp },
        { "cxx", cpp },
        { "rs", rust },
        { "rust", rust },
        { "Rust", rust },
        { "java", java },
        { "Java", java },
        { "kotlin", kotlin },
        { "Kotlin", kotlin },
        { "kt", kotlin },
        { "js", javascript },
        { "JS", javascript },
        { "javascript", javascript },
        { "JavaScript", javascript },
        { "ts", typescript },
        { "TS", typescript },
        { "TypeScript", typescript },
    };
    // clang-format on

    for (const auto& pair : languages_by_name) {
        if (name == pair.name) {
            return pair.language;
        }
    }
    return {};
}

bool Colored_HTML_Consumer::write(char c, bmd::HTML_Token_Type type)
{
    return (out << highlight_color_of(type)) && out.put(c);
}

bool Colored_HTML_Consumer::write(char c, Size count, bmd::HTML_Token_Type type)
{
    if (!(out << highlight_color_of(type))) {
        return false;
    }
    char restore_fill = out.fill(c);
    out.width(std::streamsize(count));
    bool result(out << "");
    out.fill(restore_fill);
    return result;
}

bool Colored_HTML_Consumer::write(std::string_view s, bmd::HTML_Token_Type type)
{
    return (out << highlight_color_of(type)) && out.write(s.data(), std::streamsize(s.length()));
}

bool Simple_HTML_Consumer::write(char c, bmd::HTML_Token_Type)
{
    return bool(out.put(c));
}

bool Simple_HTML_Consumer::write(char c, Size count, bmd::HTML_Token_Type)
{
    char restore_fill = out.fill(c);
    out.width(std::streamsize(count));
    bool result(out << "");
    out.fill(restore_fill);
    return result;
}

bool Simple_HTML_Consumer::write(std::string_view s, bmd::HTML_Token_Type)
{
    return bool(out.write(s.data(), std::streamsize(s.length())));
}

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

std::ostream& print_code_string(std::ostream& out, const bmd::Code_String& string, bool colors)
{
    const std::string_view text = string.get_text();
    if (!colors) {
        return out << text;
    }

    bmd::Code_String_Span previous {};
    for (bmd::Code_String_Span span : string) {
        const Size previous_end = previous.begin + previous.length;
        BIT_MANIPULATION_ASSERT(span.begin >= previous_end);
        if (previous_end != span.begin) {
            out << text.substr(previous_end, span.begin - previous_end);
        }
        out << highlight_color_of(span.type) << text.substr(span.begin, span.length) << ansi::reset;
        previous = span;
    }
    const Size last_span_end = previous.begin + previous.length;
    if (last_span_end != text.size()) {
        out << text.substr(last_span_end);
    }

    return out;
}

} // namespace bit_manipulation
