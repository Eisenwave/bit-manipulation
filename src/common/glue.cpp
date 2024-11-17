#include <ostream>

#include <common/ansi.hpp>
#include <common/glue.hpp>

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
} // namespace

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
    out.width(count);
    bool result(out << "");
    out.fill(restore_fill);
    return result;
}

bool Colored_HTML_Consumer::write(std::string_view s, bmd::HTML_Token_Type type)
{
    return (out << highlight_color_of(type)) && out.write(s.data(), s.length());
}

bool Simple_HTML_Consumer::write(char c, bmd::HTML_Token_Type)
{
    return bool(out.put(c));
}

bool Simple_HTML_Consumer::write(char c, Size count, bmd::HTML_Token_Type)
{
    char restore_fill = out.fill(c);
    out.width(count);
    bool result(out << "");
    out.fill(restore_fill);
    return result;
}

bool Simple_HTML_Consumer::write(std::string_view s, bmd::HTML_Token_Type)
{
    return bool(out.write(s.data(), s.length()));
}

} // namespace bit_manipulation
