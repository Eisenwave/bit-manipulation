#ifndef BIT_MANIPULATION_BMD_PARSED_DOCUMENT_HPP
#define BIT_MANIPULATION_BMD_PARSED_DOCUMENT_HPP

#include <memory_resource>
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "common/parse.hpp"
#include "common/source_position.hpp"
#include "common/variant.hpp"

#include "bmd/directive_type.hpp"

namespace bit_manipulation::bmd::ast {

namespace detail {
struct Base {
    Local_Source_Span m_pos;

    Local_Source_Span get_source_position() const
    {
        return m_pos;
    }
};

} // namespace detail

/// @brief A class which represents an identifier in the grammar.
struct Identifier : detail::Base {
    static inline constexpr std::string_view self_name = "Identifier";

    std::string_view m_value;

    Identifier(const Local_Source_Span& pos, std::string_view value);

    std::string_view get_value() const
    {
        return m_value;
    }
};

/// @brief A class which represents all numeric literals in the grammar.
struct Number : detail::Base {
    static inline constexpr std::string_view self_name = "Number";

    Int64 m_value;
    Literal_Type m_type;

    Number(const Local_Source_Span& pos, Int64 value, Literal_Type type);

    Int64 get_value() const
    {
        return m_value;
    }

    Literal_Type get_type() const
    {
        return m_type;
    }
};

using Value = Variant<Identifier, Number>;

inline Local_Source_Span get_source_span(const Value& node)
{
    return visit([]<typename T>(const T& v) -> const detail::Base& { return v; }, node)
        .get_source_position();
}

/// @brief A class which represents raw content.
struct Raw : detail::Base {
    static inline constexpr std::string_view self_name = "Raw";

    std::string_view m_value;

    Raw(const Local_Source_Span& pos, std::string_view value);

    std::string_view get_value() const
    {
        return m_value;
    }
};

/// @brief A class which represents the grammar rules `content`, `paragraph`, and
/// contents of blocks in general.
/// The concrete meaning depends on the enclosing directive.
struct List : detail::Base {
    static inline constexpr std::string_view self_name = "List";

    std::pmr::vector<Some_Node*> m_children;

    List(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children);

    [[nodiscard]] std::span<Some_Node*> get_children();
    [[nodiscard]] std::span<Some_Node* const> get_children() const;
    [[nodiscard]] bool empty() const;
};

/// @brief A class which represents the `directive` rule in the grammar.
/// `Directive` optionally has a `Content` or `Text` child, depending on whether the rule
/// stores `raw_content` or regular `content`.
struct Directive : detail::Base {
    static inline constexpr std::string_view self_name = "Directive";
    using Arguments = std::pmr::unordered_map<std::string_view, Value>;

    Directive_Type m_type;
    std::string_view m_identifier;

    Arguments m_arguments;
    Some_Node* m_block;

    Directive(const Local_Source_Span& pos,
              Directive_Type type,
              std::string_view identifier,
              Arguments&& args,
              ast::Some_Node* block);

    Directive_Type get_type() const
    {
        return m_type;
    }

    std::string_view get_identifier() const
    {
        return m_identifier;
    }

    /// @brief Returns the block which this directive optionally has.
    /// @return `nullptr` if there is no block, `ast::Text` for raw blocks, `ast::Content`
    /// otherwise.
    ast::Some_Node* get_block() const
    {
        return m_block;
    }

    std::span<Some_Node*> get_children()
    {
        return { &m_block, 1 };
    }
    std::span<Some_Node* const> get_children() const
    {
        return { &m_block, 1 };
    }
};

/// @brief A class which represents the `text` rule in the grammar.
struct Text : detail::Base {
    static inline constexpr std::string_view self_name = "Text";

    std::string_view m_text;

    Text(const Local_Source_Span& pos, std::string_view text);

    std::string_view get_text() const
    {
        return m_text;
    }

    std::span<Some_Node*> get_children()
    {
        return {};
    }
    std::span<Some_Node* const> get_children() const
    {
        return {};
    }
};

[[nodiscard]] inline std::span<Some_Node*> List::get_children()
{
    return m_children;
}

[[nodiscard]] inline std::span<Some_Node* const> List::get_children() const
{
    return m_children;
}

[[nodiscard]] inline bool List::empty() const
{
    return m_children.empty();
}

struct Some_Node : Variant<List, Directive, Text> {
    using Variant::Variant;
};

inline std::string_view get_node_name(const Some_Node& node)
{
    return visit([]<typename T>(const T&) { return T::self_name; }, node);
}

inline std::span<Some_Node*> get_children(Some_Node& node)
{
    return visit([]<typename T>(T& v) { return v.get_children(); }, node);
}

inline std::span<Some_Node* const> get_children(const Some_Node& node)
{
    return visit([]<typename T>(const T& v) { return v.get_children(); }, node);
}

inline Local_Source_Span get_source_span(const Some_Node& node)
{
    return visit([]<typename T>(const T& v) -> const detail::Base& { return v; }, node)
        .get_source_position();
}

} // namespace bit_manipulation::bmd::ast

#endif
