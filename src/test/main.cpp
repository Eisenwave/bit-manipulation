#include <string>

#include <gtest/gtest.h>

#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/result.hpp"

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/grammar.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(BMS_Syntax, empty_function_void)
{
    EXPECT_TRUE(test_for_success("syntax/empty_function_void.bms"));
}

TEST(BMS_Syntax, function_return_zero)
{
    EXPECT_TRUE(test_for_success("syntax/function_return_zero.bms"));
}

TEST(BMS_Syntax, global_const)
{
    EXPECT_TRUE(test_for_success("syntax/global_const.bms"));
}

TEST(Valid_BMS, assert)
{
    EXPECT_TRUE(test_for_success("assert.bms"));
}

TEST(Valid_BMS, deduction)
{
    EXPECT_TRUE(test_for_success("deduction.bms"));
}

TEST(Valid_BMS, dependent_static_assert)
{
    EXPECT_TRUE(test_for_success("dependent_static_assert.bms"));
}

TEST(Valid_BMS, identity)
{
    EXPECT_TRUE(test_for_success("identity.bms"));
}

TEST(Valid_BMS, if_expression)
{
    EXPECT_TRUE(test_for_success("if_expression.bms"));
}

TEST(Valid_BMS, loop)
{
    EXPECT_TRUE(test_for_success("loop.bms"));
}

TEST(Valid_BMS, static_assert)
{
    EXPECT_TRUE(test_for_success("static_assert.bms"));
}

TEST(Valid_BMS, void)
{
    EXPECT_TRUE(test_for_success("void.bms"));
}

TEST(BMS_Tokenize_Error, illegal_character)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/illegal_character.bms",
                                    bms::Tokenize_Error_Code::illegal_character));
}

TEST(BMS_Tokenize_Error, no_digits_following_integer_prefix)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/no_digits_following_integer_prefix.bms",
                                    bms::Tokenize_Error_Code::no_digits_following_integer_prefix));
}

TEST(BMS_Tokenize_Error, integer_suffix)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/integer_suffix.bms",
                                    bms::Tokenize_Error_Code::integer_suffix));
}

TEST(BMS_Tokenize_Error, unterminated_comment)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/unterminated_comment.bms",
                                    bms::Tokenize_Error_Code::unterminated_comment));
}

TEST(BMS_Parse_Error, global_let)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 1, .token_type = bms::Token_Type::keyword_let };
    EXPECT_TRUE(test_for_diagnostic("parse_error/global_let.bms", expectations));
}

TEST(BMS_Parse_Error, let_let)
{
    constexpr Parse_Error_Expectations expectations //
        { .rule = bms::Grammar_Rule::let_declaration,
          .line = 2,
          .token_type = bms::Token_Type::keyword_let };
    EXPECT_TRUE(test_for_diagnostic("parse_error/let_let.bms", expectations));
}

TEST(BMS_Parse_Error, let_no_type_no_init)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 2, .token_type = bms::Token_Type::semicolon };
    EXPECT_TRUE(test_for_diagnostic("parse_error/let_no_type_no_init.bms", expectations));
}

TEST(BMS_Parse_Error, nameless_function)
{
    constexpr Parse_Error_Expectations expectations //
        { .rule = bms::Grammar_Rule::function_declaration,
          .line = 1,
          .token_type = bms::Token_Type::left_parenthesis };
    EXPECT_TRUE(test_for_diagnostic("parse_error/nameless_function.bms", expectations));
}

TEST(BMS_Parse_Error, unbalanced_parentheses)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 1, .token_type = bms::Token_Type::assign };
    EXPECT_TRUE(test_for_diagnostic("parse_error/unbalanced_parentheses.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_global_const)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_global_const,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/failed_to_define_global_const.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_function)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_function,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define_function.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_parameter)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_parameter,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define_parameter.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_variable)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_variable,
          .fail_line = 4,
          .cause_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, reference_to_undefined_variable)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::reference_to_undefined_variable, .fail_line = 3 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/reference_to_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, assignment_of_undefined_variable)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assignment_of_undefined_variable, .fail_line = 3 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/assignment_of_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, call_to_undefined_function)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::call_to_undefined_function, .fail_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/call_to_undefined_function.bms", expectations));
}

TEST(BMS_Analysis_Error, width_not_integer)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_not_integer, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_not_integer.bms", expectations));
}

// FIXME: width_not_integer looks to be dead diagnostic because more specific diagnostics like
//        let_variable_in_constant_expression get triggered instead
TEST(BMS_Analysis_Error, DISABLED_width_not_const)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_not_const, .fail_line = 4, .cause_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_not_const.bms", expectations));
}

TEST(BMS_Analysis_Error, width_too_large)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_too_large, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_too_large.bms", expectations));
}

TEST(BMS_Analysis_Error, width_zero)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_zero, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_zero.bms", expectations));
}

// FIXME: expected_constant_expression might be dead diagnostic for same reason as above

TEST(BMS_Analysis_Error, let_variable_in_constant_expression)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::let_variable_in_constant_expression,
          .fail_line = 4,
          .cause_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/let_variable_in_constant_expression.bms",
                                    expectations));
}

TEST(BMS_Analysis_Error, parameter_in_constant_expression)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::parameter_in_constant_expression,
          .fail_line = 3,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/parameter_in_constant_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, function_in_expression)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::function_in_expression,
          .fail_line = 4,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/function_in_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, condition_not_bool_in_if_expression)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::condition_not_bool, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/condition_not_bool/if_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, condition_not_bool_in_if_statement)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::condition_not_bool, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/condition_not_bool/if_statement.bms", expectations));
}

TEST(BMS_Analysis_Error, invalid_integer_literal)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::invalid_integer_literal, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/invalid_integer_literal.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_parameter)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_parameter, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning_parameter.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_function)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_function, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning_function.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_const)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_const, .fail_line = 3, .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning_const.bms", expectations));
}

TEST(BMS_Analysis_Error, call_non_function)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::call_non_function, .fail_line = 3, .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/call_non_function.bms", expectations));
}

TEST(BMS_Analysis_Error, wrong_number_of_arguments)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::wrong_number_of_arguments,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/wrong_number_of_arguments.bms", expectations));
}

TEST(BMS_Analysis_Error, codegen_call_to_unanalyzed)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::codegen_call_to_unanalyzed,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/codegen_call_to_unanalyzed.bms", expectations));
}

TEST(BMS_Analysis_Error, width_deduction_from_non_uint)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_deduction_from_non_uint,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/width_deduction_from_non_uint.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assert_expression_not_bool)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assert_expression_not_bool, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/static_assert_expression_not_bool.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assertion_failed)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assertion_failed, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/static_assertion_failed.bms", expectations));
}

TEST(BMS_Analysis_Error, requires_clause_not_bool)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::requires_clause_not_bool,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/requires_clause_not_bool.bms", expectations));
}

TEST(BMS_Analysis_Error, requires_clause_not_satisfied)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::requires_clause_not_satisfied,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/requires_clause_not_satisfied.bms", expectations));
}

TEST(BMS_Analysis_Error, use_of_undefined_variable)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::use_of_undefined_variable,
          .fail_line = 3,
          .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/use_of_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, use_of_undefined_constant)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::use_of_undefined_constant,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/use_of_undefined_constant.bms", expectations));
}

TEST(BMS_Analysis_Error, empty_return_in_non_void_function)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::empty_return_in_non_void_function,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/empty_return_in_non_void_function.bms", expectations));
}

// TODO: test for continue outside loop

// TODO: test for break outside loop

TEST(BMS_Analysis_Error, void_operation)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::void_operation, .fail_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/void_operation.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_arithmetic)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_arithmetic, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_arithmetic.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_bitwise)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_bitwise, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_bitwise.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_relational_comparison)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_relational_comparison, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_relational_comparison.bms", expectations));
}

TEST(BMS_Analysis_Error, int_bitwise)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::int_bitwise, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/int_bitwise.bms", expectations));
}

TEST(BMS_Analysis_Error, non_bool_logical)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::non_bool_logical, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/non_bool_logical.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_types)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_types, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_types.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_widths)
{
    constexpr Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_widths, .fail_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_widths.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
