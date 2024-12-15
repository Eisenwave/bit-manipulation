#ifndef BIT_MANIPULATION_BMD_HTML_WRITER_HPP
#define BIT_MANIPULATION_BMD_HTML_WRITER_HPP

#include "common/assert.hpp"
#include "common/parse.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

/// @brief Returns `true` if the given string requires wrapping in quotes when it
/// appears as the value in an attribute.
/// For example, `id=123` is a valid HTML attribute with a value and requires
/// no wrapping, but `id="<x>"` requires `<x>` to be surrounded by quotes.
inline bool requires_quotes_in_attribute(std::string_view value)
{
    return value.find_first_of("\"/'`=<> ") != std::string_view::npos;
}

/// @brief The formatting style of an HTML tag.
/// For example, we like to output `<b>bold text</b>` (`Formatting_Style::in_line`) all in one line,
/// but a `<ul>` (`Formatting_Style::block`) should have its contents put on a separate line.
enum struct Formatting_Style : Default_Underlying {
    /// @brief Pre-formatted.
    /// No changes to spacing should be performed, no indentation added, etc.
    pre,
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

[[nodiscard]] constexpr bool is_block_like(Formatting_Style style)
{
    return style == Formatting_Style::flat || style == Formatting_Style::block;
}

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

/// @brief RAII helper class which lets us write attributes more conveniently.
/// This class is not intended to be used directly, but with the help of `HTML_Writer`.
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

struct Tag_Properties {
    std::string_view id;
    Formatting_Style style;
};

/// @brief A class which provides member functions for writing HTML content to a stream
/// correctly.
/// Both entire HTML documents can be written, as well as HTML snippets.
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
    Size m_indent_width;

    enum struct State {
        /// @brief The writer is currently at the start of a line.
        /// This is also the initial state.
        new_line,
        /// @brief The writer is in the middle of a line, but not in the middle
        /// of a tag, where attributes are currently written.
        normal,
        /// @brief The writer is currently writing attributes.
        attributes
    };

    State m_state = State::new_line;
    Size m_depth = 0;
    Size m_indent_depth = 0;

public:
    /// @brief Constructor.
    /// Writes nothing to the stream.
    /// `out.fail()` shall be true.
    /// @param out the output stream
    explicit HTML_Writer(HTML_Token_Consumer& out, Size indent_width);

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
    /// For whole documents should be called exactly once, prior to any other `write` functions.
    /// However, it is not required to call this.
    /// @return `*this`
    Self& write_preamble();

    /// @brief Writes an empty tag such as `<br/>` or `<hr/>`.
    /// @param properties the tag properties
    /// @return `*this`
    Self& write_empty_tag(Tag_Properties properties);

    /// @brief Writes an empty tag such as `<br/>` or `<hr/>`.
    /// @param tag the tag identifier; `is_identifier(tag)` shall be `true`
    /// @param style if `block`, the tag will be on a new line and indented
    /// @return `*this`
    Self& write_comment_tag(std::string_view comment, Formatting_Style style);

    /// @brief Writes an opening tag such as `<div>`.
    /// @param properties the tag properties
    /// @return `*this`
    Self& begin_tag(Tag_Properties properties);

    /// @brief Writes an incomplete opening tag such as `<div`.
    /// Returns an `Attribute_Writer` which must be used to write attributes (if any)
    /// and complete the opening tag.
    /// @param properties the tag properties
    /// @return `*this`
    [[nodiscard]] Attribute_Writer begin_tag_with_attributes(Tag_Properties properties);

    /// @brief Writes a closing tag, such as `</div>`.
    /// The most recent call to `begin_tag` or `begin_tag_with_attributes` shall have been made with
    /// the same arguments.
    /// @param properties the tag properties
    /// @return `*this`
    Self& end_tag(Tag_Properties properties);

    /// @brief Writes text between tags.
    /// Text characters such as `<` or `>` which interfere with HTML are converted to entities.
    /// @param text the text to write
    /// @param type if `block`, the tag will be on a new line and indented
    /// @return If `style` is `pre`, `1`;
    /// otherwise if `text` is empty, `0`.
    /// otherwise, the amount of distinct lines that the text was split onto.
    Size write_inner_text(std::string_view text, Formatting_Style type);

    Self& write_whitespace(char c, Size length);

    Self& write_source_gap(const Local_Source_Span& first,
                           const Local_Source_Span& second,
                           Formatting_Style style);

private:
    friend struct Attribute_Writer;

    Self& write_attribute(std::string_view key, std::string_view value);
    Self& end_attributes(Formatting_Style type);
    Self& end_empty_tag_attributes(Formatting_Style type);

    /// @brief Passes any text directly through to the writer, however, characters which interfere
    /// with HTML such as `<` or `>` are written as HTML entities instead.
    /// If the string contains no such entities, this function is equivalent to writing `text` to
    /// the writer directly.
    /// @param text the text to write
    void write_escaped_text(std::string_view text);

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
