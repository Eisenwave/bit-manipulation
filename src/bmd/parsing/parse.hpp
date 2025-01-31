#ifndef BIT_MANIPULATION_BMD_TOKENS_HPP
#define BIT_MANIPULATION_BMD_TOKENS_HPP

#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "common/config.hpp"
#include "common/result.hpp"
#include "common/variant.hpp"

#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Parse_Error_Code : Default_Underlying {
    /// @brief An illegal character was read.
    unexpected_character,
    /// @brief An opening block comment has no closing asterisk and slash.
    unterminated_comment,
    invalid_integer_literal,
    unexpected_eof,
    duplicate_argument,
    integer_suffix,
    invalid_directive,
    directive_must_be_empty,
    paragraph_break_in_span,
    directive_in_text_span,
    text_in_directive_list,
    directive_not_allowed,
};

struct Parse_Error {
    Parse_Error_Code code;
    Grammar_Rule rule;
    Local_Source_Position pos;
};

struct Parsed_Document {
private:
    std::string_view m_source;
    ast::Some_Node* m_root;
    std::pmr::memory_resource* m_memory;

public:
    [[nodiscard]] Parsed_Document(std::string_view source,
                                  ast::Some_Node* root,
                                  std::pmr::memory_resource* memory) noexcept
        : m_source(source)
        , m_root(root)
        , m_memory(memory)
    {
    }

    [[nodiscard]] Parsed_Document(Parsed_Document&& other) noexcept
        : m_source(other.m_source)
        , m_root(std::exchange(other.m_root, nullptr))
        , m_memory(other.m_memory)
    {
    }

    ~Parsed_Document()
    {
        delete_recursively(m_root);
    }

private:
    void delete_recursively(ast::Some_Node* node);

public:
    Parsed_Document& operator=(Parsed_Document&& other) noexcept
    {
        swap(*this, other);
        other.clear();
        return *this;
    }

    friend void swap(Parsed_Document& a, Parsed_Document& b) noexcept
    {
        std::swap(a.m_source, b.m_source);
        std::swap(a.m_root, b.m_root);
        std::swap(a.m_memory, b.m_memory);
    }

    void clear()
    {
        delete_recursively(m_root);
        m_root = nullptr;
    }

    [[nodiscard]] ast::Some_Node* get_root()
    {
        return m_root;
    }

    [[nodiscard]] const ast::Some_Node* get_root() const
    {
        return m_root;
    }

    [[nodiscard]] std::string_view extract(const Local_Source_Span& span) const
    {
        BIT_MANIPULATION_ASSERT(span.begin < m_source.length());
        BIT_MANIPULATION_ASSERT(span.end() <= m_source.length());
        return m_source.substr(span.begin, span.length);
    }
};

[[nodiscard]] Result<Parsed_Document, Parse_Error> parse(std::string_view source,
                                                         std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
