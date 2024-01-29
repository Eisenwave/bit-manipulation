#ifndef BIT_MANIPULATION_BMSCRIPT_HPP
#define BIT_MANIPULATION_BMSCRIPT_HPP

#include <memory>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

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
    // &&
    logical_and,
    // ||
    logical_or,
    // !
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
    requires_clause, // "requires", "(", expression, ")"
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
          | for_statement
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
    prefix_expression, // unary_operator, prefix_expression | postfix_expression
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

    type, // "Bool" | "Int" | uint
    uint, // "Uint", parenthesized_expression
};

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule);

namespace ast {

struct Node;

enum struct Node_Type {
    program, // {function | variable}
    function, // {parameter}, type, [requires_clause], block_statement
    parameter,
    type,
    variable,
    statement,
    if_statement,
    for_statement,
    while_statement,
    break_statement,
    continue_statement,
    return_statement,
    assignment,
    block_statement,
    expression,
    if_expression,
    binary_expression,
    prefix_expression,
    id_expression,
    primary_expression,
    function_call_expression,
    literal,
};

[[nodiscard]] std::string_view node_type_name(Node_Type t);

struct Program_Data {
    std::vector<Node> declarations;

    Program_Data(std::vector<Node>&& declarations);
};

struct Function_Data {
    std::string_view name;
    std::vector<Node> parameters;
    std::unique_ptr<Node> requires_clause;
    std::unique_ptr<Node> return_type;
    std::unique_ptr<Node> body;

    Function_Data(std::string_view name,
                  std::vector<Node>&& parameters,
                  Node&& requires_clause,
                  Node&& return_type,
                  Node&& body);

    Function_Data(std::string_view name,
                  std::vector<Node>&& parameters,
                  Node&& return_type,
                  Node&& body);
};

enum struct Type_Type { Bool, Int, Uint };

struct Let_Const_Data {

    static Let_Const_Data type_and_initializer(Token_Type let_or_const,
                                               std::string_view name,
                                               Node&& type,
                                               Node&& initializer);

    static Let_Const_Data
    initializer_only(Token_Type let_or_const, std::string_view name, Node&& initializer);

    static Let_Const_Data let_type_only(std::string_view name, Node&& type);

    std::string_view name;
    std::unique_ptr<Node> type, initializer;
    bool is_const;

private:
    Let_Const_Data(bool is_const,
                   std::string_view name,
                   std::unique_ptr<Node> type,
                   std::unique_ptr<Node> initializer);
};

struct Assignment_Data {
    std::string_view name;
    std::unique_ptr<Node> expression;

    Assignment_Data(std::string_view name, Node&& expression);
};

struct Parameter_Data {
    std::string_view name;
    std::unique_ptr<Node> expression;

    Parameter_Data(std::string_view name, Node&& type);
};

struct Return_Statement_Data {
    std::unique_ptr<Node> expression;

    Return_Statement_Data(Node&& expression);
};

struct Block_Statement_Data {
    std::vector<Node> statements;

    Block_Statement_Data(std::vector<Node>&& statements);
};

struct If_Statement_Data {
    std::unique_ptr<Node> condition, if_block, else_block;

    If_Statement_Data(Node&& condition, Node&& block);
    If_Statement_Data(Node&& condition, Node&& if_block, Node&& else_block);
};

struct While_Statement_Data {
    std::unique_ptr<Node> condition, block;

    While_Statement_Data(Node&& condition, Node&& block);
};

struct For_Statement_Data {
    std::unique_ptr<Node> init, condition, increment, block;

    For_Statement_Data(Node&& init, Node&& condition, Node&& increment, Node&& block);

    For_Statement_Data(Node&& init, Node&& condition, Node&& block);
};

struct If_Expression_Data {
    std::unique_ptr<Node> condition, left, right;

    If_Expression_Data(Node&& left, Node&& condition, Node&& right);
};

struct Binary_Expression_Data {
    std::unique_ptr<Node> left, right;
    Token_Type op;

    Binary_Expression_Data(Node&& left, Node&& right, Token_Type op);
};

struct Prefix_Expression_Data {
    std::unique_ptr<Node> operand;
    Token_Type op;

    Prefix_Expression_Data(Node&& operand, Token_Type op);
};

struct Function_Call_Expression_Data {
    std::string_view function;
    std::vector<Node> arguments;

    Function_Call_Expression_Data(std::string_view function, std::vector<Node>&& arguments);
};

struct Type_Data {
    std::unique_ptr<Node> width;
    Type_Type type;

public:
    static Type_Data make_bool();

    static Type_Data make_int();

    static Type_Data make_uint(Node&& width);

private:
    Type_Data(Type_Type type, std::unique_ptr<Node> width);
};

using Node_Data = std::variant<std::monostate,
                               Program_Data,
                               Function_Data,
                               Let_Const_Data,
                               Parameter_Data,
                               Assignment_Data,
                               If_Statement_Data,
                               While_Statement_Data,
                               For_Statement_Data,
                               Return_Statement_Data,
                               Block_Statement_Data,
                               Type_Data,
                               If_Expression_Data,
                               Binary_Expression_Data,
                               Prefix_Expression_Data,
                               Function_Call_Expression_Data>;

struct Node {
    /// The first token that belongs to this rule.
    Token token;
    /// The type of AST node.
    Node_Type type;
    /// Additional data.
    /// May be std::monostate, since some nodes only require the information stored in the token.
    Node_Data data;

    [[nodiscard]] Node(Token token, Node_Type type, Node_Data&& data = {})
        : token { token }
        , type { type }
        , data { std::move(data) }
    {
    }
};

enum class Node_Handle : Size {
    // the null handle, representing no node
    null = std::numeric_limits<Size>::max()
};

} // namespace ast

struct Parsed_Program {
    std::vector<ast::Node> nodes;
    ast::Node root_node;
};

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

using Parse_Result = std::variant<ast::Node, Parse_Error>;

Parse_Result parse(std::span<const Token> tokens, std::string_view source);

} // namespace bit_manipulation

#endif