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
    // for
    keyword_for,
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

[[nodiscard]] constexpr std::string_view token_type_name(Token_Type type) noexcept
{
    using enum Token_Type;

    switch (type) {
    case identifier: return "identifier";
    case left_parenthesis: return "left_parenthesis";
    case right_parenthesis: return "right_parenthesis";
    case decimal_literal: return "decimal_literal";
    case octal_literal: return "octal_literal";
    case hexadecimal_literal: return "hexadecimal_literal";
    case binary_literal: return "binary_literal";
    case left_brace: return "left_brace";
    case right_brace: return "right_brace";
    case block_comment: return "block_comment";
    case line_comment: return "line_comment";
    case assign: return "assign";
    case equals: return "equals";
    case not_equals: return "not_equals";
    case plus: return "plus";
    case minus: return "minus";
    case multiplication: return "multiplication";
    case division: return "division";
    case remainder: return "remainder";
    case less_than: return "less_than";
    case greater_than: return "greater_than";
    case less_or_equal: return "less_or_equal";
    case greater_or_equal: return "greater_or_equal";
    case shift_left: return "shift_left";
    case shift_right: return "shift_right";
    case bitwise_and: return "bitwise_and";
    case bitwise_or: return "bitwise_or";
    case bitwise_not: return "bitwise_not";
    case bitwise_xor: return "bitwise_xor";
    case logical_and: return "logical_and";
    case logical_or: return "logical_or";
    case logical_not: return "logical_not";
    case right_arrow: return "right_arrow";
    case double_right_arrow: return "double_right_arrow";
    case dot: return "dot";
    case colon: return "colon";
    case comma: return "comma";
    case semicolon: return "semicolon";
    case keyword_let: return "keyword_let";
    case keyword_const: return "keyword_const";
    case keyword_function: return "keyword_function";
    case keyword_for: return "keyword_for";
    case keyword_while: return "keyword_while";
    case keyword_if: return "keyword_if";
    case keyword_else: return "keyword_else";
    case keyword_uint: return "keyword_uint";
    case keyword_int: return "keyword_int";
    case keyword_bool: return "keyword_bool";
    case keyword_requires: return "keyword_requires";
    case keyword_return: return "keyword_return";
    case keyword_break: return "keyword_break";
    case keyword_continue: return "keyword_continue";
    case keyword_true: return "keyword_true";
    case keyword_false: return "keyword_false";
    }
    return "";
}

[[nodiscard]] constexpr Size token_type_length(Token_Type type) noexcept
{
    using enum Token_Type;

    switch (type) {
    case identifier:
    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal:
    case block_comment:
    case line_comment: return 0;

    case left_parenthesis:
    case right_parenthesis:
    case left_brace:
    case right_brace:
    case assign:
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case less_than:
    case greater_than:
    case bitwise_and:
    case bitwise_or:
    case bitwise_not:
    case bitwise_xor:
    case dot:
    case colon:
    case comma:
    case semicolon: return 1;

    case equals:
    case not_equals:
    case less_or_equal:
    case greater_or_equal:
    case shift_left:
    case shift_right:
    case logical_and:
    case logical_or:
    case logical_not:
    case right_arrow:
    case double_right_arrow:
    case keyword_if: return 2;

    case keyword_for:
    case keyword_let:
    case keyword_int: return 3;

    case keyword_else:
    case keyword_uint:
    case keyword_bool:
    case keyword_true: return 4;

    case keyword_const:
    case keyword_break:
    case keyword_while:
    case keyword_false: return 5;

    case keyword_return: return 6;

    case keyword_function:
    case keyword_requires:
    case keyword_continue: return 8;
    }
}

[[nodiscard]] constexpr bool is_unary_operator(Token_Type type) noexcept
{
    using enum Token_Type;
    switch (type) {
    case plus:
    case minus:
    case logical_not:
    case bitwise_not: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_literal(Token_Type type) noexcept
{
    using enum Token_Type;
    switch (type) {
    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal:
    case keyword_true:
    case keyword_false: return true;
    default: return false;
    }
}

[[nodiscard]] constexpr bool is_binary_operator(Token_Type type) noexcept
{
    using enum Token_Type;
    switch (type) {
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case equals:
    case not_equals:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal:
    case logical_and:
    case logical_or:
    case shift_left:
    case shift_right:
    case bitwise_and:
    case bitwise_or:
    case bitwise_xor: return true;
    default: return false;
    }
}

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
    if_statement, // "if", "(", expression, ")", block_statement, ["else", block_statement]
    while_statement, // "while", "(", expression, ")", block_statement;
    for_statement, // "for", "(", init_clause, ";", expression, ";", assignment, ")",
                   // block_statement
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
    uint, // "Uint", "(", expression, ")"
};

[[nodiscard]] constexpr std::string_view grammar_rule_name(Grammar_Rule rule)
{
    using enum Grammar_Rule;
    switch (rule) {
    case program: return "program";
    case program_declaration: return "program_declaration";
    case const_declaration: return "const_declaration";
    case let_declaration: return "let_declaration";
    case initializer: return "initializer";
    case function_declaration: return "function_declaration";
    case function_header: return "function_header";
    case requires_clause: return "requires_clause";
    case parameter_sequence: return "parameter_sequence";
    case parameter: return "parameter";
    case statement: return "statement";
    case assignment_statement: return "assignment_statement";
    case assignment: return "assignment";
    case break_statement: return "break_statement";
    case continue_statement: return "continue_statement";
    case return_statement: return "return_statement";
    case if_statement: return "if_statement";
    case while_statement: return "while_statement";
    case for_statement: return "for_statement";
    case init_clause: return "init_clause";
    case block_statement: return "block_statement";
    case expression: return "expression";
    case if_expression: return "if_expression";
    case binary_expression: return "binary_expression";
    case prefix_expression: return "prefix_expression";
    case postfix_expression: return "postfix_expression";
    case function_call_expression: return "function_call_expression";
    case expression_sequence: return "expression_sequence";
    case primary_expression: return "primary_expression";
    case parenthesized_expression: return "parenthesized_expression";
    case integer_literal: return "integer_literal";
    case binary_operator: return "binary_operator";
    case unary_operator: return "unary_operator";
    case type: return "type";
    case uint: return "uint";
    }
    return "";
}

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

[[nodiscard]] constexpr std::string_view node_type_name(Node_Type t)
{
    using enum Node_Type;

    switch (t) {
    case program: return "program";
    case function: return "function";
    case parameter: return "parameter";
    case type: return "type";
    case variable: return "variable";
    case statement: return "statement";
    case if_statement: return "if_statement";
    case for_statement: return "for_statement";
    case while_statement: return "while_statement";
    case break_statement: return "break_statement";
    case continue_statement: return "continue_statement";
    case return_statement: return "return_statement";
    case assignment: return "assignment";
    case block_statement: return "block_statement";
    case expression: return "expression";
    case if_expression: return "if_expression";
    case binary_expression: return "binary_expression";
    case prefix_expression: return "prefix_expression";
    case id_expression: return "id_expression";
    case primary_expression: return "primary_expression";
    case function_call_expression: return "function_call_expression";
    case literal: return "literal";
    }
    return "";
}

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

struct If_While_Statement_Data {
    std::unique_ptr<Node> condition, block;

    If_While_Statement_Data(Node&& condition, Node&& block);
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
                               For_Statement_Data,
                               If_While_Statement_Data,
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

struct Declaration : Node { };

struct Program : Node {
    std::vector<Declaration> declarations;
};

struct Expression : Node { };

} // namespace ast

enum struct Parse_Error_Code { ok, illegal_character };

struct Parse_Error {
    Grammar_Rule fail_rule;
    Token fail_token;
};

using Parse_Result = std::variant<ast::Node, Parse_Error>;

Parse_Result parse(std::span<const Token> tokens, std::string_view source);

} // namespace bit_manipulation

#endif