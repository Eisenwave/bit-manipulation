#ifndef BIT_MANIPULATION_BMS_GRAMMAR_HPP
#define BIT_MANIPULATION_BMS_GRAMMAR_HPP

#include <string_view>

#include "common/config.hpp"

namespace bit_manipulation::bms {

/// @brief A rule in the formal (context-free) grammar of the BMS language.
enum struct Grammar_Rule : Default_Underlying {
    program, // declaration, { declaration }
    program_declaration, // const_declaration | function_declaration | static_assertion
    const_declaration, // "const", identifier, [":", type], initializer
    let_declaration, // "let", identifier, ":", type, ";" | "let", identifier, [":", type],
                     // initializer, ";"
    initializer, // "=", expression
    function_declaration, // "function", identifier, function_header, block_statement
    function_header, // "(", [parameter_sequence], ")", "->", type, [requires_clause]
    requires_clause, // "requires", expression
    parameter_sequence, // parameter, { ",", parameter }
    parameter, // identifier, ":", type
    static_assertion, // "static_assert", parenthesized_expression, ";"
    statement, /*
          | let_declaration
          | const_declaration
          | static_assertion
          | assignment_statement
          | function_call_statement
          | break_statement
          | continue_statement
          | return_statement
          | if_statement
          | while_statement
          | block_statement */
    assignment_statement, // assignment, ";"
    function_call_statement, // function_call_expression, ";"
    assignment, // identifier, "=", expression
    break_statement, // "break", ";"
    continue_statement, // "continue", ";"
    return_statement, // "return", [expression], ";"
    if_statement, // "if", expression, block_statement, [else_statement]
    else_statement, // "else", (if_statement | block_statement)
    while_statement, // "while", expression, block_statement;
    init_clause, // let_declaration | assignment
    block_statement, // "{" { statement } "}"
    expression, // if_expression
    if_expression, // binary_expression, ["if", binary_expression, "else", if_expression]
    binary_expression, // comparison_expression
                       // | prefix_expression, [binary_operator, prefix_expression]
    comparison_expression, // arithmetic_expression, binary_comparison_operator,
                           // arithmetic_expression
    arithmetic_expression, // prefix_expression, [binary_arithmetic_operator, prefix_expression]
    prefix_expression, // [unary_operator], postfix_expression
    postfix_expression, // function_call_expression | primary_expression
    function_call_expression, // identifier, "(", [expression_sequence], ")"
    expression_sequence, // expression, {",", expression}
    primary_expression, // integer_literal | identifier | parenthesized_expression
    parenthesized_expression, // "(", expression, ")"

    integer_literal, // decimal_literal | hexadecimal_literal | binary_literal | octal_literal
    binary_operator, // binary_arithmetic_operator | binary_comparison_operator
                     // binary_logical_operator | binary_bitwise_operator
    binary_arithmetic_operator, // "+" | "-" | "*" | "/" | "%"
    binary_comparison_operator, // "==" | "!=" | "<" | ">" | "<=" | ">="
    binary_logical_operator, // "&&" | "||"
    binary_bitwise_operator, // "<<" | ">>" | "&" | "|" | "^"
    unary_operator, // "+" | "-" | "!" | "~"

    type, // "Bool" | "Int" | "Void" | uint
    uint, // "Uint", parenthesized_expression
};

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule);

} // namespace bit_manipulation::bms

#endif