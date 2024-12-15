#ifndef BIT_MANIPULATION_CODE_STRING_HPP
#define BIT_MANIPULATION_CODE_STRING_HPP

#include <concepts>
#include <memory_resource>
#include <string_view>
#include <utility>
#include <vector>

#include "common/code_span_type.hpp"
#include "common/source_position.hpp"

namespace bit_manipulation {

struct Code_String_Span {
    Size begin;
    Size length;
    Code_Span_Type type;

    [[nodiscard]] constexpr Size end() const
    {
        return begin + length;
    }
};

struct Code_String_Iterator;

struct Code_String {
public:
    struct Length {
        Size text_length;
        Size span_count;
    };

    using iterator = Code_String_Span*;
    using const_iterator = const Code_String_Span*;

private:
    std::pmr::vector<char> m_text;
    std::pmr::vector<Code_String_Span> m_spans;

public:
    [[nodiscard]] Code_String(std::pmr::memory_resource* memory = std::pmr::get_default_resource())
        : m_text(memory)
        , m_spans(memory)
    {
    }

    [[nodiscard]] Length get_length() const
    {
        return { .text_length = m_text.size(), .span_count = m_spans.size() };
    }

    [[nodiscard]] Size get_text_length() const
    {
        return m_text.size();
    }

    [[nodiscard]] Size get_span_count() const
    {
        return m_spans.size();
    }

    [[nodiscard]] std::string_view get_text() const
    {
        return { m_text.data(), m_text.size() };
    }

    [[nodiscard]] std::string_view get_text(const Code_String_Span& span) const
    {
        return get_text().substr(span.begin, span.length);
    }

    void resize(Length length)
    {
        m_text.resize(length.text_length);
        m_spans.resize(length.span_count);
    }

    void clear() noexcept
    {
        m_text.clear();
        m_spans.clear();
    }

    /// @brief Appends a raw range of text to the string.
    /// This is typically useful for e.g. whitespace between pieces of code.
    void append(std::string_view text)
    {
        m_text.insert(m_text.end(), text.begin(), text.end());
    }

    /// @brief Appends a raw character of text to the string.
    /// This is typically useful for e.g. whitespace between pieces of code.
    void append(char c)
    {
        m_text.push_back(c);
    }

    /// @brief Appends a raw character of text multiple times to the string.
    /// This is typically useful for e.g. whitespace between pieces of code.
    void append(Size amount, char c)
    {
        m_text.insert(m_text.end(), amount, c);
    }

    void append(std::string_view text, Code_Span_Type type)
    {
        BIT_MANIPULATION_ASSERT(!text.empty());
        m_spans.push_back({ .begin = m_text.size(), .length = text.size(), .type = type });
        m_text.insert(m_text.end(), text.begin(), text.end());
    }

    void append(char c, Code_Span_Type type)
    {
        m_spans.push_back({ .begin = m_text.size(), .length = 1, .type = type });
        m_text.push_back(c);
    }

    struct Scoped_Builder;

    /// @brief Starts building a single code span out of multiple parts which will be fused
    /// together.
    /// For example:
    /// ```
    /// string.build(Code_Span_Type::identifier)
    ///     .append("m_")
    ///     .append(name);
    /// ```
    /// @param type the type of the appended span as a whole
    Scoped_Builder build(Code_Span_Type type) &;

    [[nodiscard]] iterator begin()
    {
        return m_spans.data();
    }

    [[nodiscard]] iterator end()
    {
        return m_spans.data() + Difference(m_spans.size());
    }

    [[nodiscard]] const_iterator begin() const
    {
        return m_spans.data();
    }

    [[nodiscard]] const_iterator end() const
    {
        return m_spans.data() + Difference(m_spans.size());
    }

    [[nodiscard]] const_iterator cbegin() const
    {
        return begin();
    }

    [[nodiscard]] const_iterator cend() const
    {
        return end();
    }
};

struct [[nodiscard]] Code_String::Scoped_Builder {
private:
    Code_String& self;
    Size initial_size;
    Code_Span_Type type;

public:
    Scoped_Builder(Code_String& self, Code_Span_Type type)
        : self { self }
        , initial_size { self.m_text.size() }
        , type { type }
    {
    }

    ~Scoped_Builder() noexcept(false)
    {
        BIT_MANIPULATION_ASSERT(self.m_text.size() >= initial_size);
        Size length = self.m_text.size() - initial_size;
        if (length != 0) {
            self.m_spans.push_back({ .begin = initial_size,
                                     .length = self.m_text.size() - initial_size,
                                     .type = type });
        }
    }

    Scoped_Builder(const Scoped_Builder&) = delete;
    Scoped_Builder& operator=(const Scoped_Builder&) = delete;

    Scoped_Builder& append(char c)
    {
        self.append(c);
        return *this;
    }

    Scoped_Builder& append(std::string_view text)
    {
        self.append(text);
        return *this;
    }
};

inline Code_String::Scoped_Builder Code_String::build(Code_Span_Type type) &
{
    return { *this, type };
}

} // namespace bit_manipulation

#endif
