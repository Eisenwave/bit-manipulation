#include <ostream>

#include "common/source_position.hpp"

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

HTML_Writer::HTML_Writer(HTML_Token_Consumer& out, Size indent_width)
    : m_out(out)
    , m_indent_width(indent_width)
{
}

void HTML_Writer::break_line()
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes);

    m_out.write('\n', HTML_Token_Type::whitespace);
    m_state = State::new_line;
}

void HTML_Writer::indent(Formatting_Style style)
{
    if (m_state != State::new_line && is_block_like(style)) {
        break_line();
    }
    if (m_state == State::new_line && style != Formatting_Style::pre) {
        m_out.write(' ', m_indent_depth * m_indent_width, HTML_Token_Type::whitespace);
    }
    m_state = State::normal;
}

void HTML_Writer::write_escaped_text(std::string_view text)
{
    BIT_MANIPULATION_ASSERT(m_state == State::new_line || m_state == State::normal);

    while (!text.empty()) {
        const Size bracket_pos = text.find_first_of("<>");
        const std::string_view snippet = text.substr(0, std::min(text.length(), bracket_pos));
        m_out.write(snippet, HTML_Token_Type::inner_text);
        if (bracket_pos == std::string_view::npos) {
            break;
        }
        else if (text[bracket_pos] == '<') {
            m_out.write("&lt;", HTML_Token_Type::inner_text);
        }
        else if (text[bracket_pos] == '>') {
            m_out.write("&gt;", HTML_Token_Type::inner_text);
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Logical mistake.");
        }

        text = text.substr(bracket_pos + 1);
    }
    m_state = State::normal;
}

auto HTML_Writer::write_whitespace(char c, Size length) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes && m_state != State::initial);
    BIT_MANIPULATION_ASSERT(is_space(c));

    m_out.write(c, length, HTML_Token_Type::whitespace);
    m_state = State::normal;
    return *this;
}

auto HTML_Writer::write_preamble() -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::initial);

    m_out.write("<!", HTML_Token_Type::tag_bracket);
    m_out.write("DOCTYPE html", HTML_Token_Type::preamble);
    m_out.write(">", HTML_Token_Type::tag_bracket);
    break_line();

    return *this;
}

auto HTML_Writer::write_empty_tag(Tag_Properties properties) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_html_identifier(properties.id));

    indent(properties.style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(properties.id, HTML_Token_Type::tag_identifier);
    m_out.write("/>", HTML_Token_Type::tag_bracket);
    if (is_block_like(properties.style)) {
        break_line();
    }

    return *this;
}

auto HTML_Writer::begin_tag(Tag_Properties properties) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_html_identifier(properties.id));

    indent(properties.style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(properties.id, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);
    if (is_block_like(properties.style)) {
        break_line();
        if (properties.style == Formatting_Style::block) {
            ++m_indent_depth;
        }
    }
    ++m_depth;

    return *this;
}

Attribute_Writer HTML_Writer::begin_tag_with_attributes(Tag_Properties properties)
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_html_identifier(properties.id));

    indent(properties.style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(properties.id, HTML_Token_Type::tag_bracket);

    m_state = State::attributes;

    return { *this, properties.style };
}

auto HTML_Writer::end_tag(Tag_Properties properties) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_html_identifier(properties.id));
    BIT_MANIPULATION_ASSERT(m_depth != 0);

    if (properties.style == Formatting_Style::block) {
        --m_indent_depth;
    }
    indent(properties.style);
    m_out.write("</", HTML_Token_Type::tag_bracket);
    m_out.write(properties.id, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);
    if (is_block_like(properties.style)) {
        break_line();
    }

    --m_depth;

    return *this;
}

auto HTML_Writer::write_comment_tag(std::string_view comment, Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes);
    BIT_MANIPULATION_ASSERT(comment.find("-->") == std::string_view::npos);

    indent(style);
    m_out.write("<!--", HTML_Token_Type::tag_bracket);
    m_out.write(comment, HTML_Token_Type::comment);
    m_out.write("-->", HTML_Token_Type::tag_bracket);
    if (is_block_like(style)) {
        break_line();
    }

    return *this;
}

Size HTML_Writer::write_inner_text(std::string_view text, Formatting_Style style)
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);

    if (style == Formatting_Style::pre) {
        write_escaped_text(text);
        return 1;
    }

    Size line_count = 0;

    while (!text.empty()) {
        const Size nl_pos = text.find('\n');
        std::string_view line = text.substr(0, std::min(text.length(), nl_pos));
        indent(style);

        write_escaped_text(line);
        ++line_count;

        if (nl_pos == std::string_view::npos) {
            break;
        }
        break_line();
        text = text.substr(nl_pos + 1);
    }

    return line_count;
}

auto HTML_Writer::write_attribute(std::string_view key, std::string_view value) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(key));

    m_out.write(' ', HTML_Token_Type::whitespace);
    m_out.write(key, HTML_Token_Type::attribute_key);

    if (!value.empty()) {
        m_out.write('=', HTML_Token_Type::attribute_equal);
        if (requires_quotes_in_attribute(value)) {
            m_out.write('"', HTML_Token_Type::attribute_quote);
            m_out.write(value, HTML_Token_Type::attribute_value);
            m_out.write('"', HTML_Token_Type::attribute_quote);
        }
        else {
            m_out.write(value, HTML_Token_Type::attribute_value);
        }
    }

    return *this;
}

auto HTML_Writer::end_attributes(Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);

    m_out.write('>', HTML_Token_Type::tag_bracket);

    if (is_block_like(style)) {
        break_line();
        if (style == Formatting_Style::block) {
            ++m_indent_depth;
        }
    }
    else {
        m_state = State::normal;
    }

    ++m_depth;

    return *this;
}

auto HTML_Writer::end_empty_tag_attributes(Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);

    m_out.write("/>", HTML_Token_Type::tag_bracket);

    if (is_block_like(style)) {
        break_line();
    }
    else {
        m_state = State::normal;
    }

    return *this;
}

auto HTML_Writer::write_source_gap(const Local_Source_Span& first,
                                   const Local_Source_Span& second,
                                   Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes && m_state != State::initial);
    BIT_MANIPULATION_ASSERT(first.line < second.line
                            || (first.line == second.line && first.column <= second.column));

    if (style == Formatting_Style::pre) {
        if (first.line != second.line) {
            m_out.write('\n', second.line - first.line, HTML_Token_Type::whitespace);
            if (second.column != 0) {
                m_out.write(' ', second.column, HTML_Token_Type::whitespace);
            }
        }
        else if (first.column != second.column) {
            m_out.write(' ', second.column - first.end_column(), HTML_Token_Type::whitespace);
        }
    }
    else if (first.end() != second.begin) {
        m_out.write(' ', HTML_Token_Type::whitespace);
    }

    return *this;
}

} // namespace bit_manipulation::bmd
