#ifndef BIT_MANIPULATION_BMS_GRAMMAR_HPP
#define BIT_MANIPULATION_BMS_GRAMMAR_HPP

#include <string_view>

#include "common/config.hpp"

namespace bit_manipulation::bms {

/// @brief A rule in the formal (context-free) grammar of the BMS language.
/// See grammar.ebnf.
enum struct Grammar_Rule : Default_Underlying {
    program,
    program_declaration,
    const_declaration,
    let_declaration,
    initializer,
    function_declaration,
    function_header,
    requires_clause,
    parameter_sequence,
    parameter,
    static_assertion,
    annotation_sequence,
    annotation,
    annotation_argument_sequence,
    annotation_argument,
    statement,
    assignment_statement,
    function_call_statement,
    assignment,
    break_statement,
    continue_statement,
    return_statement,
    if_statement,
    else_statement,
    while_statement,
    init_clause,
    block_statement,
    expression,
    conversion_expression,
    if_expression,
    binary_expression,
    comparison_expression,
    arithmetic_expression,
    prefix_expression,
    postfix_expression,
    function_call_expression,
    expression_sequence,
    primary_expression,
    parenthesized_expression,
    integer_literal,
    binary_operator,
    binary_arithmetic_operator,
    binary_comparison_operator,
    binary_logical_operator,
    binary_bitwise_operator,
    unary_operator,
    type,
    uint,
};

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule);

} // namespace bit_manipulation::bms

#endif
