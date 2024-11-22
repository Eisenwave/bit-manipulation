#include "bms/analysis_error.hpp"
#include "bms/grammar.hpp"
#include "bms/parse.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

[[nodiscard]] std::string_view token_type_name(Token_Type type)
{
    using enum Token_Type;

    switch (type) {
        BIT_MANIPULATION_ENUM_STRING_CASE(identifier);
        BIT_MANIPULATION_ENUM_STRING_CASE(left_parenthesis);
        BIT_MANIPULATION_ENUM_STRING_CASE(right_parenthesis);
        BIT_MANIPULATION_ENUM_STRING_CASE(decimal_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(octal_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(hexadecimal_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(left_brace);
        BIT_MANIPULATION_ENUM_STRING_CASE(right_brace);
        BIT_MANIPULATION_ENUM_STRING_CASE(block_comment);
        BIT_MANIPULATION_ENUM_STRING_CASE(line_comment);
        BIT_MANIPULATION_ENUM_STRING_CASE(assign);
        BIT_MANIPULATION_ENUM_STRING_CASE(equals);
        BIT_MANIPULATION_ENUM_STRING_CASE(not_equals);
        BIT_MANIPULATION_ENUM_STRING_CASE(plus);
        BIT_MANIPULATION_ENUM_STRING_CASE(minus);
        BIT_MANIPULATION_ENUM_STRING_CASE(multiplication);
        BIT_MANIPULATION_ENUM_STRING_CASE(division);
        BIT_MANIPULATION_ENUM_STRING_CASE(remainder);
        BIT_MANIPULATION_ENUM_STRING_CASE(less_than);
        BIT_MANIPULATION_ENUM_STRING_CASE(greater_than);
        BIT_MANIPULATION_ENUM_STRING_CASE(less_or_equal);
        BIT_MANIPULATION_ENUM_STRING_CASE(greater_or_equal);
        BIT_MANIPULATION_ENUM_STRING_CASE(shift_left);
        BIT_MANIPULATION_ENUM_STRING_CASE(shift_right);
        BIT_MANIPULATION_ENUM_STRING_CASE(bitwise_and);
        BIT_MANIPULATION_ENUM_STRING_CASE(bitwise_or);
        BIT_MANIPULATION_ENUM_STRING_CASE(bitwise_not);
        BIT_MANIPULATION_ENUM_STRING_CASE(bitwise_xor);
        BIT_MANIPULATION_ENUM_STRING_CASE(logical_and);
        BIT_MANIPULATION_ENUM_STRING_CASE(logical_or);
        BIT_MANIPULATION_ENUM_STRING_CASE(logical_not);
        BIT_MANIPULATION_ENUM_STRING_CASE(right_arrow);
        BIT_MANIPULATION_ENUM_STRING_CASE(double_right_arrow);
        BIT_MANIPULATION_ENUM_STRING_CASE(dot);
        BIT_MANIPULATION_ENUM_STRING_CASE(colon);
        BIT_MANIPULATION_ENUM_STRING_CASE(comma);
        BIT_MANIPULATION_ENUM_STRING_CASE(semicolon);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_as);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_let);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_const);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_while);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_if);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_else);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_uint);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_int);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_void);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_requires);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_return);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_break);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_continue);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_true);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_false);
        BIT_MANIPULATION_ENUM_STRING_CASE(keyword_static_assert);
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
    case keyword_as: return "'as'";
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
    case keyword_static_assert: return "'static_assert'";
    }
    return "";
}

[[nodiscard]] std::string_view token_type_code_name(Token_Type type) noexcept
{
    using enum Token_Type;

    switch (type) {
    case left_brace: return "{";
    case right_brace: return "}";
    case block_comment: return "/*";
    case line_comment: return "//";
    case assign: return "=";
    case equals: return "==";
    case not_equals: return "!=";
    case plus: return "+";
    case minus: return "-";
    case multiplication: return "*";
    case division: return "/";
    case remainder: return "%";
    case less_than: return "<";
    case greater_than: return ">";
    case less_or_equal: return "<=";
    case greater_or_equal: return ">=";
    case shift_left: return "<<";
    case shift_right: return ">>";
    case bitwise_and: return "&";
    case bitwise_or: return "|";
    case bitwise_not: return "~";
    case bitwise_xor: return "^";
    case logical_and: return "&&";
    case logical_or: return "||";
    case logical_not: return "!";
    case right_arrow: return "->";
    case double_right_arrow: return "=>";
    case dot: return ".";
    case colon: return ":";
    case comma: return ",";
    case semicolon: return ";";
    case keyword_as: return "as";
    case keyword_let: return "let";
    case keyword_const: return "const";
    case keyword_function: return "function";
    case keyword_while: return "while";
    case keyword_if: return "if";
    case keyword_else: return "else";
    case keyword_uint: return "Uint";
    case keyword_int: return "Int";
    case keyword_bool: return "Bool";
    case keyword_void: return "Void";
    case keyword_requires: return "requires";
    case keyword_return: return "return";
    case keyword_break: return "break";
    case keyword_continue: return "continue";
    case keyword_true: return "true";
    case keyword_false: return "false";
    case keyword_static_assert: return "static_assert";
    default: return "";
    }
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
    case logical_or:
    case right_arrow:
    case double_right_arrow:
    case keyword_as:
    case keyword_if: return 2;

    case logical_and:
    case logical_not:
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

    case keyword_static_assert: return 13;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid token type");
}

[[nodiscard]] bool is_comment(Token_Type type)
{
    return type == Token_Type::line_comment || type == Token_Type::block_comment;
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

[[nodiscard]] bool is_arithmetic_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_bitwise_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case bitwise_and:
    case bitwise_or:
    case bitwise_not:
    case bitwise_xor:
    case shift_left:
    case shift_right: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_comparison_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case equals:
    case not_equals:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_logical_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case logical_not:
    case logical_and:
    case logical_or: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_relational_comparison_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal: return true;
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
        BIT_MANIPULATION_ENUM_STRING_CASE(program);
        BIT_MANIPULATION_ENUM_STRING_CASE(program_declaration);
        BIT_MANIPULATION_ENUM_STRING_CASE(const_declaration);
        BIT_MANIPULATION_ENUM_STRING_CASE(let_declaration);
        BIT_MANIPULATION_ENUM_STRING_CASE(initializer);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_declaration);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_header);
        BIT_MANIPULATION_ENUM_STRING_CASE(requires_clause);
        BIT_MANIPULATION_ENUM_STRING_CASE(parameter_sequence);
        BIT_MANIPULATION_ENUM_STRING_CASE(parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(static_assertion);
        BIT_MANIPULATION_ENUM_STRING_CASE(statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(assignment_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_call_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(assignment);
        BIT_MANIPULATION_ENUM_STRING_CASE(break_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(continue_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(return_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(if_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(else_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(while_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(init_clause);
        BIT_MANIPULATION_ENUM_STRING_CASE(block_statement);
        BIT_MANIPULATION_ENUM_STRING_CASE(expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(conversion_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(if_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(comparison_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(arithmetic_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(prefix_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(postfix_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_call_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(expression_sequence);
        BIT_MANIPULATION_ENUM_STRING_CASE(primary_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(parenthesized_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(integer_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_arithmetic_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_comparison_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_logical_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(binary_bitwise_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(unary_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(type);
        BIT_MANIPULATION_ENUM_STRING_CASE(uint);
    }
    return "";
}

} // namespace bit_manipulation::bms
