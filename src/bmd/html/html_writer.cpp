#include <ostream>

#include "common/code_string.hpp"
#include "common/source_position.hpp"

#include "bmd/html/html_writer.hpp"

namespace bit_manipulation::bmd {
namespace {

void append_escaped_text(Code_String::Scoped_Builder& builder, std::string_view text)
{
    while (!text.empty()) {
        const Size bracket_pos = text.find_first_of("<>");
        const std::string_view snippet = text.substr(0, std::min(text.length(), bracket_pos));
        builder.append(snippet);
        if (bracket_pos == std::string_view::npos) {
            break;
        }
        else if (text[bracket_pos] == '<') {
            builder.append("&lt;");
        }
        else if (text[bracket_pos] == '>') {
            builder.append("&gt;");
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Logical mistake.");
        }

        text = text.substr(bracket_pos + 1);
    }
}

} // namespace

HTML_Writer::HTML_Writer(Code_String& out)
    : m_out(out)
{
}

void HTML_Writer::write_inner_text(std::string_view text)
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);

    auto builder = m_out.build(Code_Span_Type::html_inner_text);
    append_escaped_text(builder, text);
}

void HTML_Writer::write_inner_html(std::string_view text)
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);
    m_out.append(text, Code_Span_Type::html_inner_text);
}

auto HTML_Writer::write_preamble() -> Self&
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);

    m_out.append("<!", Code_Span_Type::html_tag_bracket);
    m_out.append("DOCTYPE html", Code_Span_Type::html_preamble);
    m_out.append(">", Code_Span_Type::html_tag_bracket);
    m_out.append('\n');

    return *this;
}

auto HTML_Writer::write_empty_tag(std::string_view id) -> Self&
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(id));

    m_out.append('<', Code_Span_Type::html_tag_bracket);
    m_out.append(id, Code_Span_Type::html_tag_identifier);
    m_out.append("/>", Code_Span_Type::html_tag_bracket);

    return *this;
}

auto HTML_Writer::open_tag(std::string_view id) -> Self&
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(id));

    m_out.append('<', Code_Span_Type::html_tag_bracket);
    m_out.append(id, Code_Span_Type::html_tag_identifier);
    m_out.append('>', Code_Span_Type::html_tag_bracket);
    ++m_depth;

    return *this;
}

Attribute_Writer HTML_Writer::open_tag_with_attributes(std::string_view id)
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(id));

    m_out.append('<', Code_Span_Type::html_tag_bracket);
    m_out.append(id, Code_Span_Type::html_tag_identifier);

    return Attribute_Writer { *this };
}

auto HTML_Writer::close_tag(std::string_view id) -> Self&
{
    BIT_MANIPULATION_ASSERT(!m_in_attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(id));
    BIT_MANIPULATION_ASSERT(m_depth != 0);

    --m_depth;

    m_out.append("</", Code_Span_Type::html_tag_bracket);
    m_out.append(id, Code_Span_Type::html_tag_identifier);
    m_out.append('>', Code_Span_Type::html_tag_bracket);

    return *this;
}

auto HTML_Writer::write_comment(std::string_view comment) -> Self&
{
    auto builder = m_out.build(Code_Span_Type::html_comment);
    builder.append("<!--");
    append_escaped_text(builder, comment);
    builder.append("-->");
    return *this;
}

auto HTML_Writer::write_attribute(std::string_view key, std::string_view value) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_in_attributes);
    BIT_MANIPULATION_ASSERT(is_html_identifier(key));

    m_out.append(' ');
    m_out.append(key, Code_Span_Type::html_attribute_key);

    if (!value.empty()) {
        m_out.append('=', Code_Span_Type::html_attribute_equal);
        auto builder = m_out.build(Code_Span_Type::html_attribute_value);
        if (requires_quotes_in_attribute(value)) {
            builder.append('"');
            builder.append(value);
            builder.append('"');
        }
        else {
            builder.append(value);
        }
    }

    return *this;
}

auto HTML_Writer::end_attributes() -> Self&
{
    BIT_MANIPULATION_ASSERT(m_in_attributes);

    m_out.append('>', Code_Span_Type::html_tag_bracket);
    m_in_attributes = false;
    ++m_depth;

    return *this;
}

auto HTML_Writer::end_empty_tag_attributes() -> Self&
{
    BIT_MANIPULATION_ASSERT(m_in_attributes);

    m_out.append("/>", Code_Span_Type::html_tag_bracket);
    m_in_attributes = false;

    return *this;
}

} // namespace bit_manipulation::bmd
