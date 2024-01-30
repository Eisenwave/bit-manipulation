#ifndef BIT_MANIPULATION_BMSCRIPT_HPP
#define BIT_MANIPULATION_BMSCRIPT_HPP

#include <memory>
#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "assert.hpp"
#include "config.hpp"

namespace bit_manipulation {

/// Represents a position in a source file.
struct Source_Position {
    /// Line number.
    Size line;
    /// Column number.
    Size column;
    /// First index in the source file that is part of the syntactical element.
    Size begin;
};

enum struct Token_Type {
    // identifier
    identifier,
    // (
    left_parenthesis,
    // )
    right_parenthesis,
    // 123
    decimal_literal,
    // 0123
    octal_literal,
    // 0xff
    hexadecimal_literal,
    // 0b1010
    binary_literal,
    // {
    left_brace,
    // }
    right_brace,
    // begin of C-style comment.
    block_comment,
    // C99, aka. double-slash comment.
    line_comment,
    // =
    assign,
    // ==
    equals,
    // !=
    not_equals,
    // +
    plus,
    // -
    minus,
    // *
    multiplication,
    // /
    division,
    // %
    remainder,
    // <
    less_than,
    // >
    greater_than,
    // <=
    less_or_equal,
    // >=
    greater_or_equal,
    // <<
    shift_left,
    // >>
    shift_right,
    // &
    bitwise_and,
    // |
    bitwise_or,
    // ~
    bitwise_not,
    // ^
    bitwise_xor,
    // and
    logical_and,
    // or
    logical_or,
    // not
    logical_not,
    // ->
    right_arrow,
    // =>
    double_right_arrow,
    // .
    dot,
    // :
    colon,
    // comma
    comma,
    // ;
    semicolon,
    // let
    keyword_let,
    // const
    keyword_const,
    // function
    keyword_function,
    // while
    keyword_while,
    // if
    keyword_if,
    // else
    keyword_else,
    // Uint
    keyword_uint,
    // Int
    keyword_int,
    // Bool,
    keyword_bool,
    // Void,
    keyword_void,
    // requires
    keyword_requires,
    // return
    keyword_return,
    // break
    keyword_break,
    // continue
    keyword_continue,
    // true
    keyword_true,
    // false
    keyword_false,
};

[[nodiscard]] std::string_view token_type_name(Token_Type type);

[[nodiscard]] std::string_view token_type_readable_name(Token_Type type);

[[nodiscard]] Size token_type_length(Token_Type type);

[[nodiscard]] bool is_comment(Token_Type type);

[[nodiscard]] bool is_unary_operator(Token_Type type);

[[nodiscard]] bool is_literal(Token_Type type);

[[nodiscard]] bool is_binary_operator(Token_Type type);

struct Token {
    Source_Position pos {};
    Size length {};
    Token_Type type {};

    [[nodiscard]] Token() = default;

    [[nodiscard]] Token(Source_Position pos, Size length, Token_Type type) noexcept
        : pos { pos }
        , length { length }
        , type { type }
    {
    }

    [[nodiscard]] std::string_view extract(std::string_view source) const
    {
        return source.substr(pos.begin, length);
    }
};

enum struct Tokenize_Error_Code { ok, illegal_character };

struct Tokenize_Error {
    Tokenize_Error_Code code;
    Source_Position pos;

    [[nodiscard]] explicit operator bool() const noexcept
    {
        return code == Tokenize_Error_Code::ok;
    }
};

Tokenize_Error tokenize(std::vector<Token>& out, std::string_view source) noexcept;

enum struct Grammar_Rule {
    program, // declaration, { declaration }
    program_declaration, // const_declaration | function_declaration
    const_declaration, // "const", identifier, [":", type], initializer
    let_declaration, // "let", identifier, ":", type, ";" | "let", identifier, [":", type],
                     // initializer, ";"
    initializer, // "=", expression, ";"
    function_declaration, // "function", identifier, function_header, block_statement
    function_header, // "(", [parameter_sequence], ")", "->", type, [requires_clause]
    requires_clause, // "requires", expression
    parameter_sequence, // parameter, { ",", parameter }
    parameter, // identifier, ":", type
    statement, /* const_declaration
          | let_declaration
          | const_declaration
          | assignment_statement
          | break_statement
          | continue_statement
          | return_statement
          | if_statement
          | while_statement
          | block_statement */
    assignment_statement, // assignment, ";"
    assignment, // identifier, "=", expression
    break_statement, // "break", ";"
    continue_statement, // "continue", ";"
    return_statement, // "return", expression, ";"
    if_statement, // "if", expression, block_statement, ["else", block_statement]
    while_statement, // "while", expression, block_statement;
    init_clause, // let_declaration | assignment
    block_statement, // "{" { statement } "}"
    expression, // if_expression
    if_expression, // binary_expression, ["if", binary_expression, "else", binary_expression]
    binary_expression, // prefix_expression, [binary_operator, prefix_expression]
    prefix_expression, // [unary_operator], postfix_expression
    postfix_expression, // function_call_expression | primary_expression
    function_call_expression, // identifier, "(", [expression_sequence], ")"
    expression_sequence, // expression, {",", expression}
    primary_expression, // integer_literal | identifier | parenthesized_expression
    parenthesized_expression, // "(", expression, ")"

    integer_literal, // decimal_literal | hexadecimal_literal | binary_literal | octal_literal
    binary_operator, // "+" | "-" | "*" | "/" | "%"
                     // | "==" | "!=" | "<" | ">" | "<=" | ">="
                     // | "&&" | "||"
                     // | "<<" | ">>" | "&" | "|" | "^"
    unary_operator, // "+" | "-" | "!" | "~"

    type, // "Bool" | "Int" | "Void" | uint
    uint, // "Uint", parenthesized_expression
};

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule);

namespace ast {

/// A type which represents a handle into the AST.
/// It can be used only in conjunction with `Parsed_Program`.
/// This is basically just an index, but with more type safety and protection against misuse.
/// By only giving the user an index in the AST, it's possible to store it as a `std::vector` and
/// massively reduce the amount of allocations necessary.
enum struct Node_Handle : Size {
    // The null handle, representing no node.
    null = std::numeric_limits<Size>::max()
};

} // namespace ast

enum struct Type_Type { Void, Bool, Int, Uint };

struct Concrete_Type {
    Type_Type type;
    int width;

    constexpr Concrete_Type(Type_Type type, int width = 0)
        : type(type)
        , width(width)
    {
    }

    friend constexpr bool operator==(Concrete_Type x, Concrete_Type y)
    {
        return x.type != y.type || (x.type == Type_Type::Uint && x.width != y.width);
    }
};

struct Bit_Generic_Type {
    Type_Type type;
    ast::Node_Handle width;

    constexpr Bit_Generic_Type(Type_Type type, ast::Node_Handle width)
        : type(type)
        , width(width)
    {
        BIT_MANIPULATION_ASSERT(width != ast::Node_Handle::null);
    }
};

using Some_Type = std::variant<Concrete_Type, Bit_Generic_Type>;

struct Unknown_Value {
    Some_Type type;

    constexpr Unknown_Value(Some_Type type)
        : type(type)
    {
    }
};

struct Concrete_Value {
    Some_Type type;
    BigInt value;

    constexpr Concrete_Value(Some_Type type, BigInt value)
        : type(type)
        , value(value)
    {
    }
};

struct Abstract_Value {
    Some_Type type;
    ast::Node_Handle value;

    constexpr Abstract_Value(Some_Type type, ast::Node_Handle value)
        : type(type)
        , value(value)
    {
        BIT_MANIPULATION_ASSERT(value != ast::Node_Handle::null);
    }
};

using Some_Value = std::variant<Unknown_Value, Concrete_Value, Abstract_Value>;

inline bool is_concrete(const Concrete_Type&)
{
    return true;
}

inline bool is_concrete(const Some_Type& type)
{
    return type.index() == 0;
}

inline bool is_concrete(const Concrete_Value&)
{
    return true;
}

inline bool is_concrete(const Some_Value& type)
{
    return type.index() == 0;
}

namespace ast {

struct Node;

enum struct Node_Type {
    program, // {function | variable}
    function, // {parameter}, type, [requires_clause], block_statement
    parameter,
    type,
    variable,
    if_statement,
    while_statement,
    break_statement,
    continue_statement,
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

struct Program_Data {
    std::vector<Node_Handle> declarations;

    Program_Data(std::vector<Node_Handle>&& declarations);
};

struct Function_Data {
    std::string_view name;
    std::vector<Node_Handle> parameters;
    Node_Handle requires_clause;
    Node_Handle return_type;
    Node_Handle body;

    Function_Data(std::string_view name,
                  std::vector<Node_Handle>&& parameters,
                  Node_Handle return_type,
                  Node_Handle requires_clause,
                  Node_Handle body);
};

struct Let_Const_Data {
    std::string_view name;
    Node_Handle type, initializer;
    bool is_const;

    Let_Const_Data(Token_Type let_or_const,
                   std::string_view name,
                   Node_Handle type,
                   Node_Handle initializer);
};

struct Assignment_Data {
    std::string_view name;
    Node_Handle expression;

    Assignment_Data(std::string_view name, Node_Handle expression);
};

struct Parameter_Data {
    std::string_view name;
    Some_Type type;

    Parameter_Data(std::string_view name, Some_Type type);
};

struct Return_Statement_Data {
    Node_Handle expression;

    Return_Statement_Data(Node_Handle expression);
};

struct Block_Statement_Data {
    std::vector<Node_Handle> statements;

    Block_Statement_Data(std::vector<Node_Handle>&& statements);
};

struct If_Statement_Data {
    Node_Handle condition, if_block, else_block;

    If_Statement_Data(Node_Handle condition, Node_Handle if_block, Node_Handle else_block);
};

struct While_Statement_Data {
    Node_Handle condition, block;

    While_Statement_Data(Node_Handle condition, Node_Handle block);
};

struct If_Expression_Data {
    Node_Handle condition, left, right;

    If_Expression_Data(Node_Handle left, Node_Handle condition, Node_Handle right);
};

struct Binary_Expression_Data {
    Node_Handle left, right;
    Token_Type op;

    Binary_Expression_Data(Node_Handle left, Node_Handle right, Token_Type op);
};

struct Prefix_Expression_Data {
    Node_Handle operand;
    Token_Type op;

    Prefix_Expression_Data(Token_Type opm, Node_Handle operand);
};

struct Function_Call_Expression_Data {
    std::string_view function;
    std::vector<Node_Handle> arguments;

    Function_Call_Expression_Data(std::string_view function, std::vector<Node_Handle>&& arguments);
};

using Node_Data = std::variant<std::monostate,
                               Program_Data,
                               Function_Data,
                               Let_Const_Data,
                               Parameter_Data,
                               Assignment_Data,
                               If_Statement_Data,
                               While_Statement_Data,
                               Return_Statement_Data,
                               Block_Statement_Data,
                               If_Expression_Data,
                               Binary_Expression_Data,
                               Prefix_Expression_Data,
                               Function_Call_Expression_Data,
                               Some_Type>;

struct Node {
    /// The first token that belongs to this rule.
    Token token;
    /// The type of AST node.
    Node_Type type;
    /// Additional data.
    /// May be std::monostate, since some nodes only require the information stored in the token.
    Node_Data data;
    /// The value of this node.
    /// Depending on the type of node, this has different meaning.
    /// This information is uncovered during semantic analysis.
    std::optional<Some_Value> value;

    [[nodiscard]] Node(Token token, Node_Type type, Node_Data&& data = {})
        : token { token }
        , type { type }
        , data { std::move(data) }
    {
    }
};

} // namespace ast

struct Parsed_Program {
    std::vector<ast::Node> nodes;
    std::string_view source;
    ast::Node_Handle root_node;

    ast::Node& get_node(ast::Node_Handle handle) &
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Node_Handle::null);
        return nodes[static_cast<Size>(handle)];
    }
};

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

using Parse_Result = std::variant<Parsed_Program, Parse_Error>;

Parse_Result parse(std::span<const Token> tokens, std::string_view source);

enum struct Analysis_Error_Code {
    ok,
    failed_to_define_global_const,
    failed_to_define_function,
    failed_to_define_parameter,
    failed_to_define_variable,
    reference_to_undefined_variable,
    assignment_of_undefined_variable,
    call_to_undefined_function
};

struct Analysis_Result {
    Analysis_Error_Code code;
    Token fail_token;
    Token cause_token;

    [[nodiscard]] constexpr explicit operator bool() const noexcept
    {
        return code == Analysis_Error_Code::ok;
    }
};

Analysis_Result analyze(Parsed_Program& program);

} // namespace bit_manipulation

#endif