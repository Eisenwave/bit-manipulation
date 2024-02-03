#ifndef BIT_MANIPULATION_BMS_PARSING_HPP
#define BIT_MANIPULATION_BMS_PARSING_HPP

#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "assert.hpp"
#include "config.hpp"
#include "result.hpp"

#include "bms/fwd.hpp"
#include "bms/grammar.hpp"
#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

namespace ast {
namespace detail {

struct Node_Base {
    Token token;

    explicit Node_Base(Token token)
        : token(token)
    {
    }
};

template <int N>
struct Parent {
    Node_Handle children[N];

    std::span<Node_Handle> get_children()
    {
        return children;
    }

    std::span<const Node_Handle> get_children() const
    {
        return children;
    }
};

template <>
struct Parent<0> {

    std::span<Node_Handle> get_children()
    {
        return {};
    }

    std::span<const Node_Handle> get_children() const
    {
        return {};
    }
};

} // namespace detail

struct Program_Node final : detail::Node_Base {
    std::vector<Node_Handle> declarations;

    Program_Node(Token token, std::vector<Node_Handle>&& declarations);

    std::span<Node_Handle> get_children()
    {
        return declarations;
    }
    std::span<const Node_Handle> get_children() const
    {
        return declarations;
    }
};

struct Function_Node final : detail::Node_Base, detail::Parent<4> {
    std::string_view name;
    std::vector<Node_Handle> instances;
    std::optional<Value> const_value;
    bool is_generic = false;

    Function_Node(Token token,
                  std::string_view name,
                  Node_Handle parameters,
                  Node_Handle return_type,
                  Node_Handle requires_clause,
                  Node_Handle body);

    Node_Handle get_parameters() const
    {
        return children[0];
    }
    Node_Handle get_return_type() const
    {
        return children[1];
    }
    Node_Handle get_requires_clause() const
    {
        return children[2];
    }
    Node_Handle get_body() const
    {
        return children[3];
    }
};

struct Parameter_List_Node final : detail::Node_Base {
    std::vector<Node_Handle> parameters;
    std::optional<Value> const_value;

    Parameter_List_Node(Token token, std::vector<Node_Handle>&& parameters);

    std::span<Node_Handle> get_children()
    {
        return parameters;
    }
    std::span<const Node_Handle> get_children() const
    {
        return parameters;
    }
};

struct Parameter_Node final : detail::Node_Base, detail::Parent<1> {
    std::string_view name;
    std::optional<Value> const_value;

    Parameter_Node(Token token, std::string_view name, Node_Handle type);

    Node_Handle get_type() const
    {
        return children[0];
    }
};

template <typename T, typename U>
using const_like_t = std::conditional_t<std::is_const_v<U>, const T, T>;

struct Type_Node final : detail::Node_Base {
private:
    template <typename Self>
    static auto get_children_impl(Self& self) -> std::span<const_like_t<Node_Handle, Self>>
    {
        if (auto* g = std::get_if<Bit_Generic_Type>(&self.type)) {
            return { &g->width, &g->width + 1 };
        }
        return {};
    }

public:
    Some_Type type;

    Type_Node(Token token, Some_Type type);

    std::span<Node_Handle> get_children()
    {
        return get_children_impl(*this);
    }
    std::span<const Node_Handle> get_children() const
    {
        return get_children_impl(*this);
    }
};

struct Let_Const_Node final : detail::Node_Base, detail::Parent<2> {
    std::string_view name;
    std::optional<Value> const_value;
    bool is_const;

    Let_Const_Node(Token token,
                   Token_Type let_or_const,
                   std::string_view name,
                   Node_Handle type,
                   Node_Handle initializer);

    Node_Handle get_type() const
    {
        return children[0];
    }
    Node_Handle get_initializer() const
    {
        return children[1];
    }
};

struct If_Statement_Node final : detail::Node_Base, detail::Parent<3> {
    If_Statement_Node(Token token,
                      Node_Handle condition,
                      Node_Handle if_block,
                      Node_Handle else_block);

    Node_Handle get_condition() const
    {
        return children[0];
    }
    Node_Handle get_if_block() const
    {
        return children[1];
    }
    Node_Handle get_else_block() const
    {
        return children[2];
    }
};

struct While_Statement_Node final : detail::Node_Base, detail::Parent<2> {
    While_Statement_Node(Token token, Node_Handle condition, Node_Handle block);

    Node_Handle get_condition() const
    {
        return children[0];
    }
    Node_Handle get_block() const
    {
        return children[1];
    }
};

// break, continue
struct Jump_Node final : detail::Node_Base, detail::Parent<0> {
    Jump_Node(Token token);
};

struct Return_Statement_Node final : detail::Node_Base, detail::Parent<1> {
    std::optional<Value> const_value;

    Return_Statement_Node(Token token, Node_Handle expression);

    Node_Handle get_expression() const
    {
        return children[0];
    }
};

struct Assignment_Node final : detail::Node_Base, detail::Parent<1> {
    std::string_view name;
    Node_Handle lookup_result = Node_Handle::null;
    std::optional<Value> const_value;

    Assignment_Node(Token token, std::string_view name, Node_Handle expression);

    Node_Handle get_expression() const
    {
        return children[0];
    }
};

struct Block_Statement_Node final : detail::Node_Base {
    std::vector<Node_Handle> statements;

    Block_Statement_Node(Token token, std::vector<Node_Handle>&& statements);

    std::span<Node_Handle> get_children()
    {
        return statements;
    }
    std::span<const Node_Handle> get_children() const
    {
        return statements;
    }
};

struct If_Expression_Node final : detail::Node_Base, detail::Parent<3> {
    std::optional<Value> const_value;

    If_Expression_Node(Token token, Node_Handle left, Node_Handle condition, Node_Handle right);

    Node_Handle get_left() const
    {
        return children[0];
    }
    Node_Handle get_condition() const
    {
        return children[1];
    }
    Node_Handle get_right() const
    {
        return children[2];
    }
};

struct Binary_Expression_Node final : detail::Node_Base, detail::Parent<2> {
    Token_Type op;
    std::optional<Value> const_value;

    Binary_Expression_Node(Token token, Node_Handle left, Node_Handle right, Token_Type op);

    Node_Handle get_left() const
    {
        return children[0];
    }
    Node_Handle get_right() const
    {
        return children[1];
    }
};

struct Prefix_Expression_Node final : detail::Node_Base, detail::Parent<1> {
    Token_Type op;
    std::optional<Value> const_value;

    Prefix_Expression_Node(Token token, Token_Type opm, Node_Handle operand);

    Node_Handle get_expression() const
    {
        return children[0];
    }
};

struct Function_Call_Expression_Node final : detail::Node_Base {
    std::string_view function;
    std::vector<Node_Handle> arguments;
    Node_Handle lookup_result = Node_Handle::null;
    std::optional<Value> const_value;

    Function_Call_Expression_Node(Token token,
                                  std::string_view function,
                                  std::vector<Node_Handle>&& arguments);

    std::span<Node_Handle> get_children()
    {
        return arguments;
    }
    std::span<const Node_Handle> get_children() const
    {
        return arguments;
    }
};

struct Id_Expression_Node final : detail::Node_Base, detail::Parent<0> {
    Node_Handle lookup_result = Node_Handle::null;
    bool bit_generic = false;
    std::optional<Value> const_value;

    Id_Expression_Node(Token token);
};

struct Literal_Node final : detail::Node_Base, detail::Parent<0> {
    Literal_Node(Token token);

    std::optional<Value> const_value;
};

template <typename T>
concept Lookup_Performing_Node = requires(T& t) {
    {
        t.lookup_result
    } -> std::same_as<Node_Handle&>;
};

template <typename T>
concept Node_Concept = requires(T& n, const T& c) {
    {
        n.get_token()
    } -> std::same_as<Token>;
    {
        n.get_children()
    } -> std::same_as<std::span<Node_Handle>>;
    {
        c.get_children()
    } -> std::same_as<std::span<const Node_Handle>>;
};

using Some_Node = std::variant<Program_Node,
                               Function_Node,
                               Parameter_List_Node,
                               Parameter_Node,
                               Type_Node,
                               Let_Const_Node,
                               If_Statement_Node,
                               While_Statement_Node,
                               Jump_Node,
                               Return_Statement_Node,
                               Assignment_Node,
                               Block_Statement_Node,
                               If_Expression_Node,
                               Binary_Expression_Node,
                               Prefix_Expression_Node,
                               Function_Call_Expression_Node,
                               Id_Expression_Node,
                               Literal_Node>;

enum struct Node_Type : int {
    program,
    function,
    parameter,
    parameter_list,
    type,
    variable,
    if_statement,
    while_statement,
    jump,
    return_statement,
    assignment,
    block_statement,
    if_expression,
    binary_expression,
    prefix_expression,
    function_call_expression,
    id_expression,
    literal,
};

[[nodiscard]] std::string_view node_type_name(Node_Type t);

inline Node_Type get_node_type(Some_Node& node)
{
    return static_cast<Node_Type>(node.index());
}

inline Token get_token(Some_Node& node)
{
    return std::visit([](detail::Node_Base& n) { return n.token; }, node);
}

inline std::span<Node_Handle> get_children(Some_Node& node)
{
    return std::visit([](auto& n) { return n.get_children(); }, node);
}

inline std::span<const Node_Handle> get_children(const Some_Node& node)
{
    return std::visit([](auto& n) { return n.get_children(); }, node);
}

} // namespace ast

struct Parsed_Program {
    std::vector<ast::Some_Node> nodes;
    std::string_view source;
    ast::Node_Handle root_node;

    ast::Some_Node& get_node(ast::Node_Handle handle) &
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Node_Handle::null);
        return nodes[static_cast<Size>(handle)];
    }

    ast::Node_Handle push_node(ast::Some_Node&& node) &
    {
        const auto result = static_cast<ast::Node_Handle>(nodes.size());
        nodes.push_back(std::move(node));
        return result;
    }
};

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

Result<Parsed_Program, Parse_Error> parse(std::span<const Token> tokens, std::string_view source);

} // namespace bit_manipulation::bms

#endif