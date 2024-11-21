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
        { .code = bms::Analysis_Error_Code::width_not_integer, .fail_line = 1, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_not_integer.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
