#include "bms/analysis_error.hpp"
#include "bms/annotation.hpp"
#include "bms/expression_type.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/tokenization/token_type.hpp"

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
        BIT_MANIPULATION_ENUM_STRING_CASE(string_literal);
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
        BIT_MANIPULATION_ENUM_STRING_CASE(at);
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
    case string_literal: return "string_literal";
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
    case at: return "'@'";
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
    case string_literal:
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
    case at:
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

[[nodiscard]] bool is_integer_literal(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: return true;
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
    case keyword_false:
    case string_literal: return true;
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

[[nodiscard]] Token_Type expression_type_token(Expression_Type type)
{
    using enum Expression_Type;

    switch (type) {
    case if_expression: return Token_Type::keyword_if;
    case conversion: return Token_Type::keyword_as;
    case logical_and: return Token_Type::logical_and;
    case logical_or: return Token_Type::logical_or;
    case equals: return Token_Type::equals;
    case not_equals: return Token_Type::not_equals;
    case less_than: return Token_Type::less_than;
    case greater_than: return Token_Type::greater_than;
    case less_or_equal: return Token_Type::less_or_equal;
    case greater_or_equal: return Token_Type::greater_or_equal;
    case binary_plus: return Token_Type::plus;
    case binary_minus: return Token_Type::minus;
    case multiplication: return Token_Type::multiplication;
    case division: return Token_Type::division;
    case remainder: return Token_Type::remainder;
    case shift_left: return Token_Type::shift_left;
    case shift_right: return Token_Type::shift_right;
    case bitwise_and: return Token_Type::bitwise_and;
    case bitwise_or: return Token_Type::bitwise_or;
    case bitwise_xor: return Token_Type::bitwise_xor;
    case unary_plus: return Token_Type::plus;
    case unary_minus: return Token_Type::minus;
    case logical_not: return Token_Type::logical_not;
    case bitwise_not: return Token_Type::bitwise_not;
    case function_call: return Token_Type::keyword_function;
    case literal: return Token_Type::remainder;
    case id: return Token_Type::remainder;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid expression type.");
}

[[nodiscard]] std::string_view expression_type_code_name(Expression_Type type)
{
    return token_type_code_name(expression_type_token(type));
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
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_sequence);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_argument_sequence);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_argument);
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
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid grammar rule.");
}

[[nodiscard]] constexpr std::string_view annotation_type_name(Annotation_Type type)
{
    using enum Annotation_Type;
    switch (type) {
        BIT_MANIPULATION_ENUM_STRING_CASE(immutable);
    case inline_:
        return "inline";
        BIT_MANIPULATION_ENUM_STRING_CASE(loop_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(loop_step);
        BIT_MANIPULATION_ENUM_STRING_CASE(unroll);
        BIT_MANIPULATION_ENUM_STRING_CASE(c_equivalent);
        BIT_MANIPULATION_ENUM_STRING_CASE(java_equivalent);
        BIT_MANIPULATION_ENUM_STRING_CASE(instantiate);
        BIT_MANIPULATION_ENUM_STRING_CASE(false_if_unknown);
        BIT_MANIPULATION_ENUM_STRING_CASE(remove_if_unused);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid annotation type.");
}

[[nodiscard]] std::optional<Annotation_Type> annotation_type_by_name(std::string_view name)
{
    constexpr auto limit = Default_Underlying(Annotation_Type::remove_if_unused);
    for (Default_Underlying i = 0; i <= limit; ++i) {
        if (annotation_type_name(Annotation_Type(i)) == name) {
            return Annotation_Type(i);
        }
    }
    return {};
}

[[nodiscard]] std::string_view analysis_error_code_name(Analysis_Error_Code code)
{
    switch (code) {
        using enum Analysis_Error_Code;
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_global_const);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(failed_to_define_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(reference_to_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(assignment_of_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(call_to_undefined_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_not_integer);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_invalid);
        BIT_MANIPULATION_ENUM_STRING_CASE(expected_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(let_variable_in_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(parameter_in_constant_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(function_in_expression);
        BIT_MANIPULATION_ENUM_STRING_CASE(execution_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(evaluation_error);
        BIT_MANIPULATION_ENUM_STRING_CASE(condition_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(invalid_integer_literal);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(assigning_const);
        BIT_MANIPULATION_ENUM_STRING_CASE(call_non_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(wrong_number_of_arguments);
        BIT_MANIPULATION_ENUM_STRING_CASE(codegen_call_to_unanalyzed);
        BIT_MANIPULATION_ENUM_STRING_CASE(width_deduction_from_non_uint);
        BIT_MANIPULATION_ENUM_STRING_CASE(static_assert_expression_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(static_assertion_failed);
        BIT_MANIPULATION_ENUM_STRING_CASE(requires_clause_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(requires_clause_not_satisfied);
        BIT_MANIPULATION_ENUM_STRING_CASE(use_of_undefined_variable);
        BIT_MANIPULATION_ENUM_STRING_CASE(use_of_undefined_constant);
        BIT_MANIPULATION_ENUM_STRING_CASE(empty_return_in_non_void_function);
        BIT_MANIPULATION_ENUM_STRING_CASE(invalid_operator);
        BIT_MANIPULATION_ENUM_STRING_CASE(void_operation);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_arithmetic);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_bitwise);
        BIT_MANIPULATION_ENUM_STRING_CASE(bool_relational_comparison);
        BIT_MANIPULATION_ENUM_STRING_CASE(int_bitwise);
        BIT_MANIPULATION_ENUM_STRING_CASE(int_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(uint_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(non_bool_logical);
        BIT_MANIPULATION_ENUM_STRING_CASE(incompatible_types);
        BIT_MANIPULATION_ENUM_STRING_CASE(incompatible_widths);
        BIT_MANIPULATION_ENUM_STRING_CASE(wrong_argument_type);
        BIT_MANIPULATION_ENUM_STRING_CASE(break_outside_loop);
        BIT_MANIPULATION_ENUM_STRING_CASE(continue_outside_loop);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_unknown);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_duplicate);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_not_applicable);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_too_many_arguments);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_unknown_parameter);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_argument_duplicate);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_argument_wrong_type);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_argument_wrong_value);
        BIT_MANIPULATION_ENUM_STRING_CASE(annotation_missing_argument);
    };
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid analysis error code.");
}

} // namespace bit_manipulation::bms
