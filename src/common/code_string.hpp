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

    using iterator = Code_String_Iterator;
    using const_iterator = Code_String_Iterator;

private:
    struct Internal_Span {
        Size begin;
        Size length;
        Code_Span_Type type;
    };

    std::pmr::vector<char> m_text;
    std::pmr::vector<Internal_Span> m_spans;

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

    struct [[nodiscard]] Scoped_Builder {
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

    /// @brief Starts building a single code span out of multiple parts which will be fused
    /// together.
    /// For example:
    /// ```
    /// string.build(Code_Span_Type::identifier)
    ///     .append("m_")
    ///     .append(name);
    /// ```
    /// @param type the type of the appended span as a whole
    Scoped_Builder build(Code_Span_Type type) &
    {
        return { *this, type };
    }

    [[nodiscard]] const_iterator begin() const;

    [[nodiscard]] const_iterator end() const;

    [[nodiscard]] const_iterator cbegin() const;

    [[nodiscard]] const_iterator cend() const;

    friend struct Code_String_Iterator;

private:
    Code_String_Span extract(const Internal_Span& span) const
    {
        return { .begin = span.begin, .length = span.length, .type = span.type };
    }
};

struct Code_String_Iterator {
public:
    friend struct Code_String;

    using difference_type = Difference;
    using value_type = Code_String_Span;
    using self_type = Code_String_Iterator;

private:
    const Code_String* m_string = nullptr;
    Difference m_index = 0;

    [[nodiscard]] Code_String_Iterator(const Code_String* string, Difference index)
        : m_string(string)
        , m_index(index)
    {
    }

public:
    [[nodiscard]] Code_String_Iterator() = default;

    [[nodiscard]] value_type operator*() const
    {
        BIT_MANIPULATION_ASSERT(m_index >= 0);
        return m_string->extract(m_string->m_spans[Size(m_index)]);
    }

    [[nodiscard]] value_type operator[](Difference d) const
    {
        return *(*this + d);
    }

    self_type& operator++()
    {
        ++m_index;
        return *this;
    }

    self_type operator++(int)
    {
        auto copy = *this;
        ++*this;
        return copy;
    }

    self_type& operator--()
    {
        BIT_MANIPULATION_ASSERT(m_index != 0);
        --m_index;
        return *this;
    }

    self_type operator--(int)
    {
        auto copy = *this;
        --*this;
        return copy;
    }

    [[nodiscard]] self_type& operator+=(difference_type d) &
    {
        m_index += d;
        BIT_MANIPULATION_ASSERT(m_index >= 0);
        return *this;
    }

    [[nodiscard]] self_type& operator-=(difference_type d) &
    {
        m_index -= d;
        BIT_MANIPULATION_ASSERT(m_index >= 0);
        return *this;
    }

    [[nodiscard]] friend self_type operator+(self_type iter, difference_type d)
    {
        return iter += d;
    }

    [[nodiscard]] friend self_type operator+(difference_type d, self_type iter)
    {
        return iter += d;
    }

    [[nodiscard]] friend self_type operator-(self_type iter, difference_type d)
    {
        return iter -= d;
    }

    [[nodiscard]] friend difference_type operator-(self_type a, self_type b)
    {
        BIT_MANIPULATION_ASSERT(a.m_string == b.m_string);
        return a.m_index - b.m_index;
    }

    [[nodiscard]] friend bool operator==(self_type a, self_type b)
    {
        BIT_MANIPULATION_ASSERT(a.m_string == b.m_string);
        return a.m_index == b.m_index;
    }

    [[nodiscard]] friend std::strong_ordering operator<=>(self_type a, self_type b)
    {
        BIT_MANIPULATION_ASSERT(a.m_string == b.m_string);
        return a.m_index <=> b.m_index;
    }
};

inline auto Code_String::begin() const -> const_iterator
{
    return { this, 0 };
}

inline auto Code_String::end() const -> const_iterator
{
    return { this, Difference(m_spans.size()) };
}

inline auto Code_String::cbegin() const -> const_iterator
{
    return begin();
}

inline auto Code_String::cend() const -> const_iterator
{
    return end();
}

} // namespace bit_manipulation

#endif