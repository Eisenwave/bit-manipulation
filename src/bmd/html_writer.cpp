#include <ostream>

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

HTML_Writer::HTML_Writer(HTML_Token_Consumer& out)
    : m_out(out)
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
    if (m_state == State::new_line) {
        m_out.write_indent(m_indent_depth);
    }
    m_state = State::normal;
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

auto HTML_Writer::write_empty_tag(std::string_view tag, Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    indent(style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write("/>", HTML_Token_Type::tag_bracket);
    if (is_block_like(style)) {
        break_line();
    }

    return *this;
}

auto HTML_Writer::begin_tag(std::string_view tag, Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    indent(style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);
    if (is_block_like(style)) {
        break_line();
        if (style == Formatting_Style::block) {
            ++m_indent_depth;
        }
    }
    ++m_depth;

    return *this;
}

Attribute_Writer HTML_Writer::begin_tag_with_attributes(std::string_view tag,
                                                        Formatting_Style style)
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    indent(style);
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_bracket);

    m_state = State::attributes;

    return { *this, style };
}

auto HTML_Writer::end_tag(std::string_view tag, Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    BIT_MANIPULATION_ASSERT(m_depth != 0);

    if (style == Formatting_Style::block) {
        --m_indent_depth;
    }
    indent(style);
    m_out.write("</", HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);
    if (is_block_like(style)) {
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

auto HTML_Writer::write_inner_text(std::string_view text, Formatting_Style style) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal || m_state == State::new_line);

    while (!text.empty()) {
        const Size nl_pos = text.find('\n');
        std::string_view line = text.substr(0, std::min(text.length(), nl_pos));
        indent(style);

        while (!line.empty()) {
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

            line = line.substr(bracket_pos + 1);
        }

        if (nl_pos == std::string_view::npos) {
            break;
        }
        break_line();
        text = text.substr(nl_pos + 1);
    }

    return *this;
}

auto HTML_Writer::write_attribute(std::string_view key, std::string_view value) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    BIT_MANIPULATION_ASSERT(is_identifier(key));

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

} // namespace bit_manipulation::bmd
