#ifndef BIT_MANIPULATION_BMD_TOKENS_HPP
#define BIT_MANIPULATION_BMD_TOKENS_HPP

#include <span>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "common/config.hpp"
#include "common/result.hpp"

#include "common/source_position.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

// clang-format off
enum struct Grammar_Rule {
    document, // = [blank | paragraph_break], [content];
    content, // = paragraph, { paragraph_break, [paragraph] };
    paragraph, // = { (directive | text), [blank] };
    paragraph_break, // = (* whitespace/comment sequence containing a whitespace line outside comments *);
    text, // = (* longest sequence of characters that does not include a "\\" (other than for escape sequences) or paragraph_break *);
    directive, // = "\\", identifier, [arguments], [block];
    arguments, // = "[", { [blank], argument, [blank], [","] }, [blank] "]";
    block, // = "{", raw_content | ([blank | paragraph_break], content), "}";
    raw_content, // = (* longest brace-balanced sequence, including whitespace and comments *)
    argument, // = identifier, [blank], "=", [blank], value;
    value, // = binary_literal | octal_literal | decimal_literal | hexadecimal_literal | identifier;
    binary_literal, // = "0b", ("0" | "1"), {"0" | "1"};
    octal_literal, // = "0", (* octal digit *), {(* octal digit *)};
    decimal_literal, // = (* decimal digit *), {(* decimal digit *)};
    hexadecimal_literal, // "0x", (* hexadecimal digit *), {(* hexadecimal digit *)};
    identifier, // /[_a-zA-Z][_a-zA-Z0-9]*/;
    blank, // = (* C89-style comment, C99-style comment, or whitespace sequence *)
};
// clang-format on

namespace ast {

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
    std::string_view m_value;

    Identifier(const Local_Source_Span& pos, std::string_view value);

    std::string_view get_value() const
    {
        return m_value;
    }
};

/// @brief A class which represents all numeric literals in the grammar.
struct Number : detail::Base {
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

using Value_Variant = std::variant<Identifier, Number>;

/// @brief `Identifier` or `Number`.
struct Value : Value_Variant {
    using Value_Variant::variant;
};

/// @brief A class which represents raw content.
struct Raw : detail::Base {
    std::string_view m_value;

    Raw(const Local_Source_Span& pos, std::string_view value);

    std::string_view get_value() const
    {
        return m_value;
    }
};

/// @brief A class which represents the `content` grammar rule.
/// `Content` has N `Paragraph` children.
struct Content : detail::Base {
    std::pmr::vector<Some_Node*> m_children;

    Content(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children);

    std::span<Some_Node*> get_children()
    {
        return m_children;
    }
    std::span<Some_Node* const> get_children() const
    {
        return m_children;
    }
};

/// @brief A class which represents the `paragraph` grammar rule.
/// `Paragraph` has N `Directive` or `Text` children.
struct Paragraph : detail::Base {
    std::pmr::vector<Some_Node*> m_children;

    Paragraph(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children);

    std::span<Some_Node*> get_children()
    {
        return m_children;
    }
    std::span<Some_Node* const> get_children() const
    {
        return m_children;
    }
};

/// @brief A class which represents the `directive` rule in the grammar.
/// `Directive` optionally has a `Content` or `Text` child, depending on whether the rule
/// stores `raw_content` or regular `content`.
struct Directive : detail::Base {
    using Arguments = std::pmr::unordered_map<std::string_view, Value>;
    std::string_view m_identifier;
    Arguments m_arguments;
    Some_Node* m_block;

    Directive(const Local_Source_Span& pos,
              std::string_view identifier,
              Arguments&& args,
              ast::Some_Node* block);

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

using AST_Variant = std::variant<Content, Paragraph, Directive, Text>;

struct Some_Node : AST_Variant {
    using AST_Variant::variant;
};

} // namespace ast

enum struct Parse_Error_Code : Default_Underlying {
    /// @brief An illegal character was read.
    unexpected_character,
    /// @brief An opening block comment has no closing asterisk and slash.
    unterminated_comment,
    invalid_integer_literal,
    unexpected_eof,
    duplicate_argument,
    integer_suffix,
};

struct Parse_Error {
    Parse_Error_Code code;
    Grammar_Rule rule;
    Local_Source_Position pos;
};

struct Parsed_Program {
    std::string_view source;
    ast::Some_Node* root_node;
};

Result<Parsed_Program, Parse_Error> parse(std::string_view source,
                                          std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif