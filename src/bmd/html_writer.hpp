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

enum struct HTML_Tag_Type { block, in_line };

struct Attribute_Writer {
private:
    HTML_Writer& m_writer;
    HTML_Tag_Type m_type;

public:
    Attribute_Writer(HTML_Writer& writer, HTML_Tag_Type type)
        : m_writer(writer)
        , m_type(type)
    {
    }

    Attribute_Writer(const Attribute_Writer&) = delete;
    Attribute_Writer& operator=(const Attribute_Writer&) = delete;

    Attribute_Writer& write_attribute(std::string_view key, std::string_view value);

    Attribute_Writer& end();
    Attribute_Writer& end_empty();

    ~Attribute_Writer();
};

/// @brief A class which provides member functions for writing HTML content to a stream
/// correctly.
/// This writer only performs checks that are possibly without additional memory.
/// These include:
/// - verifying that given tag names and values are appropriate
/// - ensuring that the number of opened tags matches the number of closed tags
/// - ensuring that the preamble is written exactly once
struct HTML_Writer {
public:
    using Self = HTML_Writer;

    struct Config {
        Size indent_width;
    };

private:
    std::ostream& m_out;
    Size m_indent_width;
    enum struct State { initial, normal, attributes } m_state = State::initial;
    Size m_depth = 0;
    Size m_indent_depth = 0;

public:
    HTML_Writer(std::ostream& out, const Config& config);

    HTML_Writer(const HTML_Writer&) = delete;
    HTML_Writer& operator=(const HTML_Writer&) = delete;

    ~HTML_Writer();

    Self& write_preamble(std::string_view tag);

    Self& write_empty_tag(std::string_view tag, HTML_Tag_Type type);

    Self& write_comment_tag(std::string_view comment, HTML_Tag_Type type);

    Self& begin_tag(std::string_view tag, HTML_Tag_Type type);

    [[nodiscard]] Attribute_Writer begin_tag_with_attributes(std::string_view tag,
                                                             HTML_Tag_Type type);

    Self& end_tag(std::string_view tag, HTML_Tag_Type type);

    Self& write_inner_text(std::string_view text);

    [[nodiscard]] operator bool() const;

private:
    friend struct Attribute_Writer;

    Self& write_attribute(std::string_view key, std::string_view value);
    Self& end_attributes(HTML_Tag_Type type);
    Self& end_empty_tag_attributes(HTML_Tag_Type type);

    void write(std::string_view raw);
    void write(char c);
    void write_indent(Size level);
};

inline Attribute_Writer& Attribute_Writer::write_attribute(std::string_view key,
                                                           std::string_view value)
{
    m_writer.write_attribute(key, value);
    return *this;
}

inline Attribute_Writer& Attribute_Writer::end()
{
    m_writer.end_attributes(m_type);
    return *this;
}

inline Attribute_Writer& Attribute_Writer::end_empty()
{
    m_writer.end_empty_tag_attributes(m_type);
    return *this;
}

inline Attribute_Writer::~Attribute_Writer()
{
    // This indicates that end() or end_empty() weren't called.
    BIT_MANIPULATION_ASSERT(m_writer.m_state != HTML_Writer::State::attributes);
}

} // namespace bit_manipulation::bmd

#endif
