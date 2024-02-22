#ifndef BIT_MANIPULATION_BMD_HTML_WRITER_HPP
#define BIT_MANIPULATION_BMD_HTML_WRITER_HPP

#include <iosfwd>

#include "common/assert.hpp"
#include "common/parse.hpp"

namespace bit_manipulation::bmd {

inline bool requires_quotes_in_attribute(std::string_view value)
{
    return value.find_first_of("\"'`=<> ") != std::string_view::npos;
}

/// @brief A class which provides member functions for writing HTML content to a stream
/// correctly.
/// This writer does not ensure that the HTML document is structurally correct.
/// It only performs basic checks such as verifying that tag names are identifiers, etc.
/// It also decides whether to use quotes or unquoted values for tag attributes.
struct HTML_Writer {
public:
    using Self = HTML_Writer;

private:
    std::ostream& m_out;
    enum struct State { normal, attributes } m_state = State::normal;
    Size m_depth = 0;

public:
    HTML_Writer(std::ostream& out);

    Self& write_preamble(std::string_view tag)
    {
        write("<!DOCTYPE html>\n");
        return *this;
    }

    Self& write_empty_tag_no_attributes(std::string_view tag);

    Self& begin_tag_no_attributes(std::string_view tag);

    Self& begin_tag_with_attributes(std::string_view tag);

    Self& write_attribute(std::string_view key, std::string_view value);

    Self& end_attributes()
    {
        m_state = State::normal;
        write('>');
        return *this;
    }

    Self& end_empty_tag_attributes()
    {
        m_state = State::normal;
        write("/>");
        return *this;
    }

    Self& close_tag(std::string_view tag);

    Self& write_inner_text(std::string_view text)
    {
        BIT_MANIPULATION_ASSERT(text.find_first_of("<>") == std::string_view::npos);
        write(text);
        return *this;
    }

    Self& write_inner_text_line(std::string_view text)
    {
        write_inner_text(text);
        write('\n');
        return *this;
    }

    [[nodiscard]] operator bool() const;

private:
    Self& write(std::string_view raw);

    Self& write(char c);
};

} // namespace bit_manipulation::bmd

#endif
