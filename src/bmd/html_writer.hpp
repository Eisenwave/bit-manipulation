#ifndef BIT_MANIPULATION_BMD_HTML_WRITER_HPP
#define BIT_MANIPULATION_BMD_HTML_WRITER_HPP

#include "common/assert.hpp"
#include "common/parse.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

inline bool requires_quotes_in_attribute(std::string_view value)
{
    return value.find_first_of("\"'`=<> ") != std::string_view::npos;
}

enum struct Formatting_Style : Default_Underlying {
    /// @brief No separate line and no extra indentation.
    /// This is typically used for tags such as `<span>`.
    in_line,
    /// @brief Separate line but no indentation.
    /// This is typically used for tags such as `<html>`.
    flat,
    /// @brief Separate line and extra indentation.
    /// This is typically used for tags such as `<p>`.
    block,
};

enum struct HTML_Token_Type : Default_Underlying {
    whitespace,
    preamble,
    comment,
    tag_identifier,
    tag_bracket,
    attribute_key,
    attribute_equal,
    attribute_comma,
    attribute_quote,
    attribute_value,
    inner_text,
};

/// @brief The polymorphic token consumer.
/// This class' member functions are invoked by `HTML_Writer` with the text content and the
/// token type to write.
/// The token type can be ignored, but may be used to output with syntax highlighting.
struct HTML_Token_Consumer {
    virtual bool write(char c, HTML_Token_Type type) = 0;
    virtual bool write(std::string_view s, HTML_Token_Type type) = 0;
    virtual bool write_indent(Size indent_level) = 0;
};

struct Attribute_Writer {
private:
    HTML_Writer& m_writer;
    Formatting_Style m_type;

public:
    Attribute_Writer(HTML_Writer& writer, Formatting_Style type)
        : m_writer(writer)
        , m_type(type)
    {
    }

    Attribute_Writer(const Attribute_Writer&) = delete;
    Attribute_Writer& operator=(const Attribute_Writer&) = delete;

    /// @brief Writes an attribute to the stream, such as `class=centered`.
    /// If `value` is empty, writes `key` on its own.
    /// If `value` requires quotes to comply with the HTML standard, quotes are added.
    /// For example, if `value` is `x y`, `key="x y"` is written.
    /// @param key the attribute key; `is_identifier(key)` shall be `true`.
    /// @param value the attribute value, or an empty string
    /// @return `*this`
    Attribute_Writer& write_attribute(std::string_view key, std::string_view value = "");

    /// @brief Writes `>` and finishes writing attributes.
    /// This function or `end_empty()` shall be called exactly once prior to destruction of this
    /// writer.
    /// @return `*this`
    Attribute_Writer& end();

    /// @brief Writes `/>` and finishes writing attributes.
    /// This function or `end()` shall be called exactly once prior to destruction of this
    /// writer.
    /// @return `*this`
    Attribute_Writer& end_empty();

    /// @brief Destructor.
    /// A call to `end()` or `end_empty()` shall have been made prior to destruction.
    ~Attribute_Writer() noexcept(false);
};

/// @brief A class which provides member functions for writing HTML content to a stream
/// correctly.
/// This writer only performs checks that are possibly without additional memory.
/// These include:
/// - verifying that given tag names and values are appropriate
/// - ensuring that the number of opened tags matches the number of closed tags
/// - ensuring that the preamble is written exactly once
///
/// To correctly use this class, the opening tags must match the closing tags.
/// I.e. for every `begin_tag(tag, type)` or `begin_tag_with_attributes(tag, type)`,
/// there must be a matching `end_tag(tag, type)`.
struct HTML_Writer {
public:
    using Self = HTML_Writer;

private:
    HTML_Token_Consumer& m_out;
    enum struct State { initial, attributes, normal, new_line } m_state = State::initial;
    Size m_depth = 0;
    Size m_indent_depth = 0;

public:
    /// @brief Constructor.
    /// Writes nothing to the stream.
    /// `out.fail()` shall be true.
    /// @param out the output stream
    explicit HTML_Writer(HTML_Token_Consumer& out);

    HTML_Writer(const HTML_Writer&) = delete;
    HTML_Writer& operator=(const HTML_Writer&) = delete;

    /// @brief Validates whether the HTML document is complete.
    /// Namely, `write_preamble()` must have taken place and any opened tags must have
    /// been closed.
    /// This member function has false negatives, however, it has no false positives.
    /// @return `true` if the writer is in a state where the HTML document could be considered
    /// complete, `false` otherwise.
    bool is_done() const
    {
        return (m_state != State::normal || m_state == State::new_line) && m_depth == 0
            && m_indent_depth == 0;
    }

    /// @brief Writes the `<!DOCTYPE ...` preamble for the HTML file.
    /// This function must be called exactly once, prior to any other `write` functions.
    /// @return `*this`
    Self& write_preamble();

    /// @brief Writes an empty tag such as `<br/>` or `<hr/>`.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& write_empty_tag(std::string_view tag, Formatting_Style type);

    /// @brief Writes an empty tag such as `<br/>` or `<hr/>`.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& write_comment_tag(std::string_view comment, Formatting_Style type);

    /// @brief Writes an opening tag such as `<div>`.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& begin_tag(std::string_view tag, Formatting_Style type);

    /// @brief Writes an incomplete opening tag such as `<div`.
    /// Returns an `Attribute_Writer` which must be used to write attributes (if any)
    /// and complete the opening tag.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    [[nodiscard]] Attribute_Writer begin_tag_with_attributes(std::string_view tag,
                                                             Formatting_Style type);

    /// @brief Writes a closing tag, such as `</div>`.
    /// The most recent call to `begin_tag` or `begin_tag_with_attributes` shall have been made with
    /// the same arguments.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& end_tag(std::string_view tag, Formatting_Style type);

    /// @brief Writes text between tags.
    /// This text shall not include any `<` or `>` characters.
    /// Use `&lt; and `&gt;` if need be.
    /// @param text the text to write
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& write_inner_text(std::string_view text, Formatting_Style type);

private:
    friend struct Attribute_Writer;

    Self& write_attribute(std::string_view key, std::string_view value);
    Self& end_attributes(Formatting_Style type);
    Self& end_empty_tag_attributes(Formatting_Style type);

    void break_line();
    void indent(Formatting_Style style);
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

inline Attribute_Writer::~Attribute_Writer() noexcept(false)
{
    // This indicates that end() or end_empty() weren't called.
    BIT_MANIPULATION_ASSERT(m_writer.m_state != HTML_Writer::State::attributes);
}

} // namespace bit_manipulation::bmd

#endif
