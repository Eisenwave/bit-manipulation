#ifndef BIT_MANIPULATION_BMS_AST_HPP
#define BIT_MANIPULATION_BMS_AST_HPP

#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "bms/analysis_error.hpp"
#include "bms/concrete_type.hpp"
#include "bms/deduction.hpp"
#include "bms/fwd.hpp"
#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms::ast {

namespace detail {

struct Node_Base {
    /// @brief A token which is representative of the current AST node.
    /// For example, this is the token of the operator for binary expressions.
    /// This member is important for diagnostics because it indicates where a failure occurred if
    /// it is related to some AST Node.
    Token token;
    /// @brief If present, indicates that type analysis for an AST node is complete.
    /// For some nodes such as expression nodes, this also indicates the value of the node, if it
    /// is a constant.
    std::optional<Value> const_value;

    explicit Node_Base(Token token)
        : token(token)
    {
    }
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
    static inline constexpr std::string_view self_name = "Program";

    std::vector<Handle> declarations;

    Program(Token token, std::vector<Handle>&& declarations);

    std::span<Handle> get_children()
    {
        return declarations;
    }
    std::span<const Handle> get_children() const
    {
        return declarations;
    }
};

struct Function final : detail::Node_Base, detail::Parent<4> {
    static inline constexpr std::string_view self_name = "Function";
    static inline constexpr std::string_view child_names[]
        = { "parameters", "return_type", "requires_clause", "body" };

    struct Instance {
        std::vector<int> widths;
        Handle handle;

        bool has_widths(const Widths& w) const noexcept
        {
            for (Size i = 0; i < widths.size(); ++i) {
                if (widths[i] != get_width(w, i)) {
                    return false;
                }
            }
            return true;
        }
    };

    static inline constexpr Size invalid_vm_address = Size(-1);

    std::string_view name;
    std::vector<Instance> instances;
    bool is_generic = false;
    Size vm_address = invalid_vm_address;
    Analysis_Level analysis_so_far = Analysis_Level::unanalyzed;

    Function(Token token,
             std::string_view name,
             Handle parameters,
             Handle return_type,
             Handle requires_clause,
             Handle body);

    Handle get_parameters() const
    {
        return children[0];
    }
    Handle get_return_type() const
    {
        return children[1];
    }
    Handle get_requires_clause() const
    {
        return children[2];
    }
    Handle get_body() const
    {
        return children[3];
    }
    Function copy_for_instantiation() const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        return { token, name, children[0], children[1], children[2], children[3] };
    }
    const Instance* find_instance(const Widths& w) const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        for (const Instance& instance : instances) {
            if (instance.has_widths(w)) {
                return &instance;
            }
        }
        return nullptr;
    }
};

struct Parameter_List final : detail::Node_Base {
    static inline constexpr std::string_view self_name = "Parameter_List";

    std::vector<Handle> parameters;

    Parameter_List(Token token, std::vector<Handle>&& parameters);

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
    static inline constexpr std::string_view self_name = "Parameter";
    static inline constexpr std::string_view child_names[] = { "type" };

    std::string_view name;

    Parameter(Token token, std::string_view name, Handle type);

    Handle get_type() const
    {
        return children[0];
    }
};

struct Type final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Type";
    static inline constexpr std::string_view child_names[] = { "width" };

    Type_Type type;
    std::optional<int> concrete_width;

    Type(Token token, Type_Type type, Handle width);

    Handle get_width() const
    {
        return children[0];
    }

    /// @brief Returns a concrete type based on the information in this node, or `std::nullopt`
    /// if the information is incomplete.
    /// Namely, if the type is `Uint`, a concrete width must be known, otherwise this function
    /// always succeeds.
    /// @return A concrete type, or `std::nullopt`.
    std::optional<Concrete_Type> concrete_type() noexcept
    {
        if (type == Type_Type::Uint) {
            if (concrete_width) {
                return Concrete_Type::Uint(*concrete_width);
            }
            return std::nullopt;
        }
        // For anything but UInt, why would concrete_width be set?
        BIT_MANIPULATION_ASSERT(!concrete_width);
        return Concrete_Type { type };
    }
};

struct Const final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Const";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };

    std::string_view name;

    Const(Token token, std::string_view name, Handle type, Handle initializer);

    Handle get_type() const
    {
        return children[0];
    }
    Handle get_initializer() const
    {
        return children[1];
    }
};

struct Let final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Let";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };

    std::string_view name;

    Let(Token token, std::string_view name, Handle type, Handle initializer);

    Handle get_type() const
    {
        return children[0];
    }
    Handle get_initializer() const
    {
        return children[1];
    }
};

struct Static_Assert final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Static_Assert";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Static_Assert(Token token, Handle expression);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct If_Statement final : detail::Node_Base, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Statement";
    static inline constexpr std::string_view child_names[] = {
        "condition",
        "if_block",
        "else_block",
    };

    If_Statement(Token token, Handle condition, Handle if_block, Handle else_block);

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
    static inline constexpr std::string_view self_name = "While_Statement";
    static inline constexpr std::string_view child_names[] = { "condition", "block" };

    While_Statement(Token token, Handle condition, Handle block);

    Handle get_condition() const
    {
        return children[0];
    }
    Handle get_block() const
    {
        return children[1];
    }
};

// break, continue
struct Jump final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Jump";

    Jump(Token token);
};

struct Return_Statement final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Return_Statement";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Return_Statement(Token token, Handle expression);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Assignment final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Assignment";
    static inline constexpr std::string_view child_names[] = { "expression" };

    std::string_view name;
    Handle lookup_result = Handle::null;

    Assignment(Token token, std::string_view name, Handle expression);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Block_Statement final : detail::Node_Base {
    static inline constexpr std::string_view self_name = "Block_Statement";

    std::vector<Handle> statements;

    Block_Statement(Token token, std::vector<Handle>&& statements);

    std::span<Handle> get_children()
    {
        return statements;
    }
    std::span<const Handle> get_children() const
    {
        return statements;
    }
};

struct If_Expression final : detail::Node_Base, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "condition", "right" };

    If_Expression(Token token, Handle left, Handle condition, Handle right);

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
    static inline constexpr std::string_view self_name = "Binary_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "right" };

    Token_Type op;

    Binary_Expression(Token token, Handle left, Handle right, Token_Type op);

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
    static inline constexpr std::string_view self_name = "Prefix_Expression";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Token_Type op;

    Prefix_Expression(Token token, Token_Type opm, Handle operand);

    Handle get_expression() const
    {
        return children[0];
    }
};

struct Function_Call_Expression final : detail::Node_Base {
    static inline constexpr std::string_view self_name = "Function_Call_Expression";

    std::string_view function;
    std::vector<Handle> arguments;
    Handle lookup_result = Handle::null;

    Function_Call_Expression(Token token,
                             std::string_view function,
                             std::vector<Handle>&& arguments);

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
    static inline constexpr std::string_view self_name = "Id_Expression";

    Handle lookup_result = Handle::null;
    bool bit_generic = false;

    Id_Expression(Token token);
};

struct Literal final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Literal";

    Literal(Token token);
};

using Some_Node = std::variant<Program,
                               Function,
                               Parameter_List,
                               Parameter,
                               Type,
                               Const,
                               Let,
                               Static_Assert,
                               If_Statement,
                               While_Statement,
                               Jump,
                               Return_Statement,
                               Assignment,
                               Block_Statement,
                               If_Expression,
                               Binary_Expression,
                               Prefix_Expression,
                               Function_Call_Expression,
                               Id_Expression,
                               Literal>;

inline Token get_token(const Some_Node& node)
{
    return std::visit([](const detail::Node_Base& n) { return n.token; }, node);
}

inline std::string_view get_node_name(const Some_Node& node)
{
    return std::visit([]<typename T>(const T&) { return T::self_name; }, node);
}

inline std::optional<Value>& get_const_value(Some_Node& node)
{
    return std::visit([](detail::Node_Base& n) -> auto& { return n.const_value; }, node);
}

inline std::optional<Value> get_const_value(const Some_Node& node)
{
    return std::visit([](const detail::Node_Base& n) { return n.const_value; }, node);
}

inline std::span<Handle> get_children(Some_Node& node)
{
    return std::visit([](auto& n) { return n.get_children(); }, node);
}

inline std::span<const Handle> get_children(const Some_Node& node)
{
    return std::visit([](auto& n) { return n.get_children(); }, node);
}

} // namespace bit_manipulation::bms::ast

#endif