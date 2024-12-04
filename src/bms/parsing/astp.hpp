#ifndef BIT_MANIPULATION_BMS_ASTP_HPP
#define BIT_MANIPULATION_BMS_ASTP_HPP

#include <span>
#include <string_view>
#include <type_traits>
#include <vector>

#include "common/variant.hpp"

#include "bms/fwd.hpp"
#include "bms/parsing/attribute.hpp"
#include "bms/tokenization/token.hpp"

/// @brief Namespace containing all the abstract syntax tree (AST) nodes used during parsing.
/// These nodes are later converted to regular AST nodes, which don't rely on storage in a single
/// vector.
namespace bit_manipulation::bms::astp {

namespace detail {

struct Node_Base {
    Local_Source_Span pos;
};

template <int N>
struct Parent {
    Handle children[N];

    std::span<Handle> get_children()
    {
        return children;
    }

    std::span<const Handle> get_children() const
    {
        return children;
    }
};

template <>
struct Parent<0> {

    std::span<Handle> get_children()
    {
        return {};
    }

    std::span<const Handle> get_children() const
    {
        return {};
    }
};

} // namespace detail

struct Program final : detail::Node_Base {
    using AST_Node = ast::Program;
    static inline constexpr std::string_view self_name = "Program";

    std::pmr::vector<Handle> declarations;

    Program(Local_Source_Span pos, std::pmr::vector<Handle>&& declarations);

    Program(Program&&) noexcept = default;
    Program& operator=(Program&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return declarations;
    }
    std::span<const Handle> get_children() const
    {
        return declarations;
    }
};

struct Function final : detail::Node_Base, detail::Parent<5> {
    using AST_Node = ast::Function;
    static inline constexpr std::string_view self_name = "Function";
    static inline constexpr std::string_view child_names[]
        = { "annotations", "parameters", "return_type", "requires_clause", "body" };

    std::string_view name;

    Function(Local_Source_Span pos,
             std::string_view name,
             Handle annotations,
             Handle parameters,
             Handle return_type,
             Handle requires_clause,
             Handle body);

    Handle get_attributes() const
    {
        return children[0];
    }
    Handle get_parameters() const
    {
        return children[1];
    }
    Handle get_return_type() const
    {
        return children[2];
    }
    Handle get_requires_clause() const
    {
        return children[3];
    }
    Handle get_body() const
    {
        return children[4];
    }
};

struct Parameter_List final : detail::Node_Base {
    using AST_Node = ast::Parameter_List;
    static inline constexpr std::string_view self_name = "Parameter_List";

    std::pmr::vector<Handle> parameters;

    Parameter_List(Local_Source_Span pos, std::pmr::vector<Handle>&& parameters);

    Parameter_List(Parameter_List&&) noexcept = default;
    Parameter_List& operator=(Parameter_List&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return parameters;
    }
    std::span<const Handle> get_children() const
    {
        return parameters;
    }
};

struct Parameter final : detail::Node_Base, detail::Parent<1> {
    using AST_Node = ast::Parameter;
    static inline constexpr std::string_view self_name = "Parameter";
    static inline constexpr std::string_view child_names[] = { "type" };

    std::string_view name;

    Parameter(Local_Source_Span pos, std::string_view name, Handle type);

    Handle get_type() const
    {
        return children[0];
    }
};

struct Type final : detail::Node_Base, detail::Parent<1> {
    using AST_Node = ast::Type;
    static inline constexpr std::string_view self_name = "Type";
    static inline constexpr std::string_view child_names[] = { "width" };

    Type_Type type;

    Type(Local_Source_Span pos, Type_Type type, Handle width);

    Handle get_width() const
    {
        return children[0];
    }
};

struct Const final : detail::Node_Base, detail::Parent<3> {
    using AST_Node = ast::Const;
    static inline constexpr std::string_view self_name = "Const";
    static inline constexpr std::string_view child_names[]
        = { "annotations", "type", "initializer" };

    std::string_view name;

    Const(Local_Source_Span pos,
          std::string_view name,
          Handle annotations,
          Handle type,
          Handle initializer);

    Handle get_attributes() const
    {
        return children[0];
    }
    Handle get_type() const
    {
        return children[0];
    }
    Handle get_initializer() const
    {
        return children[1];
    }
};

struct Let final : detail::Node_Base, detail::Parent<3> {
    using AST_Node = ast::Let;
    static inline constexpr std::string_view self_name = "Let";
    static inline constexpr std::string_view child_names[]
        = { "annotations", "type", "initializer" };

    std::string_view name;

    Let(Local_Source_Span pos,
        std::string_view name,
        Handle annotations,
        Handle type,
        Handle initializer);

    Handle get_attributes() const
    {
        return children[0];
    }
    Handle get_type() const
    {
        return children[1];
    }
    Handle get_initializer() const
    {
        return children[2];
    }
};

struct Static_Assert final : detail::Node_Base, detail::Parent<1> {
    using AST_Node = ast::Static_Assert;
    static inline constexpr std::string_view self_name = "Static_Assert";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Static_Assert(Local_Source_Span pos, Handle expression);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Annotation_List final : detail::Node_Base {
    static inline constexpr std::string_view self_name = "Attribute_List";

    std::pmr::vector<Handle> annotations;

    Annotation_List(Local_Source_Span pos, std::pmr::vector<Handle>&& parameters);

    Annotation_List(Annotation_List&&) noexcept = default;
    Annotation_List& operator=(Annotation_List&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return annotations;
    }
    std::span<const Handle> get_children() const
    {
        return annotations;
    }
};

struct Annotation final : detail::Node_Base {
    static inline constexpr std::string_view self_name = "Attribute";
    static inline constexpr std::string_view child_names[] = { "type" };

    std::string_view name;
    std::pmr::vector<Handle> arguments;

    Annotation(Local_Source_Span pos, std::string_view name, std::pmr::vector<Handle>&& arguments);

    Annotation(Annotation&&) noexcept = default;
    Annotation& operator=(Annotation&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return arguments;
    }
    std::span<const Handle> get_children() const
    {
        return arguments;
    }
};

struct Annotation_Argument final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Annotation_Argument";

    std::string_view value;

    Annotation_Argument(Local_Source_Span pos, std::string_view value);
};

struct If_Statement final : detail::Node_Base, detail::Parent<3> {
    using AST_Node = ast::If_Statement;
    static inline constexpr std::string_view self_name = "If_Statement";
    static inline constexpr std::string_view child_names[] = {
        "condition",
        "if_block",
        "else_block",
    };

    If_Statement(Local_Source_Span pos, Handle condition, Handle if_block, Handle else_block);

    Handle get_condition() const
    {
        return children[0];
    }
    Handle get_if_block() const
    {
        return children[1];
    }
    Handle get_else_block() const
    {
        return children[2];
    }
};

struct While_Statement final : detail::Node_Base, detail::Parent<2> {
    using AST_Node = ast::While_Statement;
    static inline constexpr std::string_view self_name = "While_Statement";
    static inline constexpr std::string_view child_names[] = { "condition", "block" };

    While_Statement(Local_Source_Span pos, Handle condition, Handle block);

    Handle get_condition() const
    {
        return children[0];
    }
    Handle get_block() const
    {
        return children[1];
    }
};

struct Break final : detail::Node_Base, detail::Parent<0> {
    using AST_Node = ast::Break;
    static inline constexpr std::string_view self_name = "Break";

    Break(Local_Source_Span pos);
};

struct Continue final : detail::Node_Base, detail::Parent<0> {
    using AST_Node = ast::Continue;
    static inline constexpr std::string_view self_name = "Continue";

    Continue(Local_Source_Span pos);
};

struct Return_Statement final : detail::Node_Base, detail::Parent<1> {
    using AST_Node = ast::Return_Statement;
    static inline constexpr std::string_view self_name = "Return_Statement";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Return_Statement(Local_Source_Span pos, Handle expression);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Assignment final : detail::Node_Base, detail::Parent<2> {
    using AST_Node = ast::Assignment;
    static inline constexpr std::string_view self_name = "Assignment";
    static inline constexpr std::string_view child_names[] = { "annotations", "expression" };

    std::string_view name;

    Assignment(Local_Source_Span pos, std::string_view name, Handle annotations, Handle expression);

    Handle get_attributes() const
    {
        return children[0];
    }
    Handle get_expression() const
    {
        return children[1];
    }
};

struct Block_Statement final : detail::Node_Base {
    using AST_Node = ast::Block_Statement;
    static inline constexpr std::string_view self_name = "Block_Statement";

    std::pmr::vector<Handle> statements;

    Block_Statement(Local_Source_Span pos, std::pmr::vector<Handle>&& statements);

    Block_Statement(Block_Statement&&) noexcept = default;
    Block_Statement& operator=(Block_Statement&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return statements;
    }
    std::span<const Handle> get_children() const
    {
        return statements;
    }
};

struct Conversion_Expression final : detail::Node_Base, detail::Parent<2> {
    using AST_Node = ast::Conversion_Expression;
    static inline constexpr std::string_view self_name = "Conversion_Expression";
    static inline constexpr std::string_view child_names[] = { "expression", "target_type" };

    Conversion_Expression(Local_Source_Span pos, Handle expression, Handle target_type);

    Handle get_expression() const
    {
        return children[0];
    }
    Handle get_target_type() const
    {
        return children[1];
    }
};

struct If_Expression final : detail::Node_Base, detail::Parent<3> {
    using AST_Node = ast::If_Expression;
    static inline constexpr std::string_view self_name = "If_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "condition", "right" };

    If_Expression(Local_Source_Span pos, Handle left, Handle condition, Handle right);

    Handle get_left() const
    {
        return children[0];
    }
    Handle get_condition() const
    {
        return children[1];
    }
    Handle get_right() const
    {
        return children[2];
    }
};

struct Binary_Expression final : detail::Node_Base, detail::Parent<2> {
    using AST_Node = ast::Binary_Expression;
    static inline constexpr std::string_view self_name = "Binary_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "right" };

    Token_Type op;

    Binary_Expression(Local_Source_Span pos, Handle left, Handle right, Token_Type op);

    Handle get_left() const
    {
        return children[0];
    }
    Handle get_right() const
    {
        return children[1];
    }
};

struct Prefix_Expression final : detail::Node_Base, detail::Parent<1> {
    using AST_Node = ast::Prefix_Expression;
    static inline constexpr std::string_view self_name = "Prefix_Expression";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Token_Type op;

    Prefix_Expression(Local_Source_Span pos, Token_Type opm, Handle operand);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Function_Call_Expression final : detail::Node_Base {
    using AST_Node = ast::Function_Call_Expression;
    static inline constexpr std::string_view self_name = "Function_Call_Expression";

    std::string_view function;
    std::pmr::vector<Handle> arguments;
    bool is_statement;

    Function_Call_Expression(Local_Source_Span pos,
                             std::string_view function,
                             std::pmr::vector<Handle>&& arguments,
                             bool is_statement = false);

    Function_Call_Expression(Function_Call_Expression&&) noexcept = default;
    Function_Call_Expression& operator=(Function_Call_Expression&&) noexcept = default;

    std::span<Handle> get_children()
    {
        return arguments;
    }
    std::span<const Handle> get_children() const
    {
        return arguments;
    }
};

struct Id_Expression final : detail::Node_Base, detail::Parent<0> {
    using AST_Node = ast::Id_Expression;
    static inline constexpr std::string_view self_name = "Id_Expression";

    std::string_view identifier;

    Id_Expression(Local_Source_Span pos, std::string_view identifier);
};

struct Literal final : detail::Node_Base, detail::Parent<0> {
    using AST_Node = ast::Literal;
    static inline constexpr std::string_view self_name = "Literal";

    std::string_view literal;
    Token_Type type;

    Literal(Local_Source_Span pos, std::string_view literal, Token_Type type);
};

using Some_Node_Variant = Variant<Program,
                                  Function,
                                  Parameter_List,
                                  Parameter,
                                  Type,
                                  Const,
                                  Let,
                                  Static_Assert,
                                  Annotation_List,
                                  Annotation,
                                  Annotation_Argument,
                                  If_Statement,
                                  While_Statement,
                                  Break,
                                  Continue,
                                  Return_Statement,
                                  Assignment,
                                  Block_Statement,
                                  Conversion_Expression,
                                  If_Expression,
                                  Binary_Expression,
                                  Prefix_Expression,
                                  Function_Call_Expression,
                                  Id_Expression,
                                  Literal>;

static_assert(Some_Node_Variant::alternatives == 25);

struct Some_Node : Some_Node_Variant {
    using Variant::Variant;
};

static_assert(std::is_nothrow_move_constructible_v<Some_Node>);
static_assert(std::is_nothrow_move_assignable_v<Some_Node>);

inline Local_Source_Span get_source_position(const Some_Node& node)
{
    return visit([](const detail::Node_Base& n) -> const auto& { return n; }, node).pos;
}

inline std::string_view get_node_name(const Some_Node& node)
{
    return visit([]<typename T>(const T&) { return T::self_name; }, node);
}

inline std::span<Handle> get_children(Some_Node& node)
{
    return visit([](auto& n) { return n.get_children(); }, node);
}

inline std::span<const Handle> get_children(const Some_Node& node)
{
    return visit([](auto& n) { return n.get_children(); }, node);
}

} // namespace bit_manipulation::bms::astp

#endif
