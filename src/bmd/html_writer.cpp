#include <ostream>

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

HTML_Writer::HTML_Writer(HTML_Token_Consumer& out)
    : m_out(out)
{
}

HTML_Writer::~HTML_Writer()
{
    BIT_MANIPULATION_ASSERT(m_depth == 0);
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
}

/*
void HTML_Writer::write_indent(Size level)
{
    char restore_fill = m_out.fill(' ');
    m_out.width(level * m_indent_width);
    m_out << "";
    m_out.fill(restore_fill);
}
*/

auto HTML_Writer::write_preamble() -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::initial);
    m_state = State::normal;
    m_out.write("<!", HTML_Token_Type::tag_bracket);
    m_out.write("DOCTYPE html", HTML_Token_Type::preamble);
    m_out.write(">", HTML_Token_Type::tag_bracket);
    return *this;
}

auto HTML_Writer::write_empty_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
        m_out.write_indent(m_indent_depth);
    }
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write("/>", HTML_Token_Type::tag_bracket);

    return *this;
}

auto HTML_Writer::begin_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
        m_out.write_indent(m_indent_depth++);
    }
    ++m_depth;
    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);

    return *this;
}

Attribute_Writer HTML_Writer::begin_tag_with_attributes(std::string_view tag, HTML_Tag_Type type)
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
        m_out.write_indent(m_indent_depth++);
    }
    ++m_depth;
    m_state = State::attributes;

    m_out.write('<', HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_bracket);

    return { *this, type };
}

auto HTML_Writer::end_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    BIT_MANIPULATION_ASSERT(m_depth != 0);

    if (type == HTML_Tag_Type::block) {
        BIT_MANIPULATION_ASSERT(m_indent_depth != 0);
        m_out.write('\n', HTML_Token_Type::whitespace);
        m_out.write_indent(m_indent_depth--);
    }
    --m_depth;
    m_state = State::normal;
    m_out.write("</", HTML_Token_Type::tag_bracket);
    m_out.write(tag, HTML_Token_Type::tag_identifier);
    m_out.write('>', HTML_Token_Type::tag_bracket);
    return *this;
}

auto HTML_Writer::write_comment_tag(std::string_view comment, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes);
    BIT_MANIPULATION_ASSERT(comment.find("-->") == std::string_view::npos);
    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
        m_out.write_indent(m_indent_depth);
    }
    m_out.write("<!--", HTML_Token_Type::tag_bracket);
    m_out.write(comment, HTML_Token_Type::comment);
    m_out.write("-->\n", HTML_Token_Type::tag_bracket);
    return *this;
}

auto HTML_Writer::write_inner_text(std::string_view text) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(text.find_first_of("<>") == std::string_view::npos);
    m_state = State::normal;
    m_out.write(text, HTML_Token_Type::inner_text);
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
            m_out.write('"', HTML_Token_Type::quote);
            m_out.write(value, HTML_Token_Type::attribute_value);
            m_out.write('"', HTML_Token_Type::attribute_quote);
        }
        else {
            m_out.write(value, HTML_Token_Type::attribute_value);
        }
    }
    return *this;
}

auto HTML_Writer::end_attributes(HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    m_state = State::normal;
    m_out.write('>', HTML_Token_Type::tag_bracket);
    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
    }
    return *this;
}

auto HTML_Writer::end_empty_tag_attributes(HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    m_state = State::normal;
    m_out.write("/>", HTML_Token_Type::tag_bracket);
    if (type == HTML_Tag_Type::block) {
        m_out.write('\n', HTML_Token_Type::whitespace);
    }
    return *this;
}

} // namespace bit_manipulation::bmd
