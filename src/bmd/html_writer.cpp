#include <ostream>

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

HTML_Writer::HTML_Writer(std::ostream& out, const Config& config)
    : m_out(out)
    , m_indent_width(config.indent_width)
{
    BIT_MANIPULATION_ASSERT(!out.fail());
}

HTML_Writer::~HTML_Writer()
{
    BIT_MANIPULATION_ASSERT(m_depth == 0);
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
}

void HTML_Writer::write(std::string_view raw)
{
    m_out.write(raw.data(), static_cast<std::streamsize>(raw.size()));
}

void HTML_Writer::write(char c)
{
    m_out.put(c);
}

void HTML_Writer::write_indent(Size level)
{
    char restore_fill = m_out.fill(' ');
    m_out.width(level * m_indent_width);
    m_out << "";
    m_out.fill(restore_fill);
}

auto HTML_Writer::write_preamble() -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::initial);
    m_state = State::normal;
    write("<!DOCTYPE html>");
    return *this;
}

auto HTML_Writer::write_empty_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        write('\n');
        write_indent(m_indent_depth);
    }
    write('<');
    write(tag);
    write("/>");

    return *this;
}

auto HTML_Writer::begin_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        write('\n');
        write_indent(m_indent_depth++);
    }
    ++m_depth;
    write('<');
    write(tag);
    write('>');

    return *this;
}

Attribute_Writer HTML_Writer::begin_tag_with_attributes(std::string_view tag, HTML_Tag_Type type)
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));

    if (type == HTML_Tag_Type::block) {
        write('\n');
        write_indent(m_indent_depth++);
    }
    ++m_depth;
    m_state = State::attributes;

    write('<');
    write(tag);

    return { *this, type };
}

auto HTML_Writer::end_tag(std::string_view tag, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    BIT_MANIPULATION_ASSERT(m_depth != 0);

    if (type == HTML_Tag_Type::block) {
        BIT_MANIPULATION_ASSERT(m_indent_depth != 0);
        write('\n');
        write_indent(m_indent_depth--);
    }
    --m_depth;
    m_state = State::normal;
    write("</");
    write(tag);
    write('>');
    return *this;
}

auto HTML_Writer::write_comment_tag(std::string_view comment, HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state != State::attributes);
    BIT_MANIPULATION_ASSERT(comment.find("-->") == std::string_view::npos);
    if (type == HTML_Tag_Type::block) {
        write('\n');
        write_indent(m_indent_depth);
    }
    write("<!--");
    write(comment);
    write("-->\n");
    return *this;
}

auto HTML_Writer::write_inner_text(std::string_view text) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(text.find_first_of("<>") == std::string_view::npos);
    m_state = State::normal;
    write(text);
    return *this;
}

[[nodiscard]] HTML_Writer::operator bool() const
{
    return bool(m_out);
}

auto HTML_Writer::write_attribute(std::string_view key, std::string_view value) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    BIT_MANIPULATION_ASSERT(is_identifier(key));
    write(' ');
    write(key);
    if (!value.empty()) {
        write('=');
        if (requires_quotes_in_attribute(value)) {
            write('"');
            write(value);
            write('"');
        }
        else {
            write(value);
        }
    }
    return *this;
}

auto HTML_Writer::end_attributes(HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    m_state = State::normal;
    write(type == HTML_Tag_Type::block ? ">\n" : ">");
    return *this;
}

auto HTML_Writer::end_empty_tag_attributes(HTML_Tag_Type type) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    m_state = State::normal;
    write(type == HTML_Tag_Type::block ? "/>\n" : "/>");
    return *this;
}

} // namespace bit_manipulation::bmd
