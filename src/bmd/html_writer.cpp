#include <ostream>

#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

HTML_Writer::HTML_Writer(std::ostream& out)
    : m_out(out)
{
    BIT_MANIPULATION_ASSERT(!out.fail());
}

auto HTML_Writer::write(std::string_view raw) -> Self&
{
    m_out << raw;
    return *this;
}

auto HTML_Writer::write(char c) -> Self&
{
    m_out << c;
    return *this;
}

auto HTML_Writer::write_empty_tag_no_attributes(std::string_view tag) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    m_out << '<' << tag << "/>";
    return *this;
}

auto HTML_Writer::begin_tag_no_attributes(std::string_view tag) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    m_out << '<' << tag << '>';
    return *this;
}

auto HTML_Writer::begin_tag_with_attributes(std::string_view tag) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::normal);
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    m_out << '<' << tag;
    m_state = State::attributes;
    return *this;
}

auto HTML_Writer::write_attribute(std::string_view key, std::string_view value) -> Self&
{
    BIT_MANIPULATION_ASSERT(m_state == State::attributes);
    BIT_MANIPULATION_ASSERT(is_identifier(key));
    m_out << ' ' << key << '=';
    if (requires_quotes_in_attribute(value)) {
        m_out << '"' << value << '"';
    }
    else {
        m_out << value;
    }
    return *this;
}

auto HTML_Writer::close_tag(std::string_view tag) -> Self&
{
    BIT_MANIPULATION_ASSERT(is_identifier(tag));
    m_out << "</" << tag << '>';
    return *this;
}

[[nodiscard]] HTML_Writer::operator bool() const
{
    return bool(m_out);
}

} // namespace bit_manipulation::bmd
