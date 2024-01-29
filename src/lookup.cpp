#include "bmscript.hpp"

namespace bit_manipulation {

[[nodiscard]] std::string_view token_type_name(Token_Type type)
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
    case keyword_while: return "keyword_while";
    case keyword_if: return "keyword_if";
    case keyword_else: return "keyword_else";
    case keyword_uint: return "keyword_uint";
    case keyword_int: return "keyword_int";
    case keyword_bool: return "keyword_bool";
    case keyword_void: return "keyword_void";
    case keyword_requires: return "keyword_requires";
    case keyword_return: return "keyword_return";
    case keyword_break: return "keyword_break";
    case keyword_continue: return "keyword_continue";
    case keyword_true: return "keyword_true";
    case keyword_false: return "keyword_false";
    }
    return "";
}

[[nodiscard]] std::string_view token_type_readable_name(Token_Type type)
{
    using enum Token_Type;

    switch (type) {
    case identifier: return "identifier";
    case left_parenthesis: return "'('";
    case right_parenthesis: return "')'";
    case decimal_literal: return "decimal literal";
    case octal_literal: return "octal literal";
    case hexadecimal_literal: return "hexadecimal literal";
    case binary_literal: return "binary literal";
    case left_brace: return "'{'";
    case right_brace: return "'}'";
    case block_comment: return "'/*'";
    case line_comment: return "'//'";
    case assign: return "'='";
    case equals: return "'=='";
    case not_equals: return "'!='";
    case plus: return "'+'";
    case minus: return "'-'";
    case multiplication: return "'*'";
    case division: return "'/'";
    case remainder: return "'%'";
    case less_than: return "'<'";
    case greater_than: return "'>'";
    case less_or_equal: return "'<='";
    case greater_or_equal: return "'>='";
    case shift_left: return "'<<'";
    case shift_right: return "'>>'";
    case bitwise_and: return "'&'";
    case bitwise_or: return "'|'";
    case bitwise_not: return "'~'";
    case bitwise_xor: return "'^'";
    case logical_and: return "'&&'";
    case logical_or: return "'||'";
    case logical_not: return "'!'";
    case right_arrow: return "'->'";
    case double_right_arrow: return "'=>'";
    case dot: return "'.'";
    case colon: return "':'";
    case comma: return "','";
    case semicolon: return "';'";
    case keyword_let: return "'let'";
    case keyword_const: return "'const'";
    case keyword_function: return "'function'";
    case keyword_while: return "'while'";
    case keyword_if: return "'if'";
    case keyword_else: return "'else'";
    case keyword_uint: return "'Uint'";
    case keyword_int: return "'Int'";
    case keyword_bool: return "'Bool'";
    case keyword_void: return "'Void'";
    case keyword_requires: return "'requires'";
    case keyword_return: return "'return'";
    case keyword_break: return "'break'";
    case keyword_continue: return "'continue'";
    case keyword_true: return "'true'";
    case keyword_false: return "'false'";
    }
    return "";
}

[[nodiscard]] Size token_type_length(Token_Type type)
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

    case keyword_let:
    case keyword_int: return 3;

    case keyword_else:
    case keyword_uint:
    case keyword_bool:
    case keyword_void:
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

[[nodiscard]] bool is_comment(Token_Type type)
{
    return type == Token_Type::line_comment || type == Token_Type::block_comment;
}

[[nodiscard]] bool is_unary_operator(Token_Type type)
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

[[nodiscard]] bool is_literal(Token_Type type)
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

[[nodiscard]] bool is_binary_operator(Token_Type type)
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

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule)
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

[[nodiscard]] constexpr std::string_view node_type_name(ast::Node_Type t)
{
    using enum ast::Node_Type;

    switch (t) {
    case program: return "program";
    case function: return "function";
    case parameter: return "parameter";
    case type: return "type";
    case variable: return "variable";
    case statement: return "statement";
    case if_statement: return "if_statement";
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

}