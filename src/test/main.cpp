#include <string>

#include <gtest/gtest.h>

#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/result.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(BMS_Syntax, annotated_everything)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_everything.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, annotated_function_calls)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_function_calls.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, annotated_loop)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_loop.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, annotation_with_arguments)
{
    EXPECT_TRUE(test_for_success("syntax/annotation_with_arguments.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, empty_function_void)
{
    EXPECT_TRUE(test_for_success("syntax/empty_function_void.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, function_return_zero)
{
    EXPECT_TRUE(test_for_success("syntax/function_return_zero.bms", Testing_Stage::parse));
}

TEST(BMS_Syntax, global_const)
{
    EXPECT_TRUE(test_for_success("syntax/global_const.bms", Testing_Stage::parse));
}

TEST(Valid_BMS, assert)
{
    EXPECT_TRUE(test_for_success("valid_bms/assert.bms"));
}

TEST(Valid_BMS, consteval_return_conversion)
{
    constexpr auto introspect = [](const bms::Analyzed_Program& program) {
        {
            Result<const bms::ast::Const*, bms::Introspection_Error_Code> x
                = program.find_global_constant("x");
            BIT_MANIPULATION_ASSERT(x);

            auto type = (*x)->const_value().value().get_type();
            if (type != bms::Concrete_Type::Uint(32)) {
                std::cout << ansi::red << "x is not of type Uint(32)\n";
                return false;
            }
        }
        {
            Result<const bms::ast::Const*, bms::Introspection_Error_Code> z
                = program.find_global_constant("z");
            BIT_MANIPULATION_ASSERT(z);

            auto type = (*z)->const_value().value().get_type();
            if (type != bms::Concrete_Type::Uint(32)) {
                std::cout << ansi::red << "z is not of type Uint(32)\n";
                return false;
            }
        }

        return true;
    };
    EXPECT_TRUE(
        test_for_success_then_introspect("valid_bms/consteval_return_conversion.bms", introspect));
}

TEST(Valid_BMS, deduction)
{
    EXPECT_TRUE(test_for_success("valid_bms/deduction.bms"));
}

TEST(Valid_BMS, dependent_static_assert)
{
    EXPECT_TRUE(test_for_success("valid_bms/dependent_static_assert.bms"));
}

TEST(Valid_BMS, identity)
{
    EXPECT_TRUE(test_for_success("valid_bms/identity.bms"));
}

TEST(Valid_BMS, if_expression)
{
    EXPECT_TRUE(test_for_success("valid_bms/if_expression.bms"));
}

TEST(Valid_BMS, loop)
{
    EXPECT_TRUE(test_for_success("valid_bms/loop.bms"));
}

TEST(Valid_BMS, return_variable)
{
    EXPECT_TRUE(test_for_success("valid_bms/return_variable.bms"));
}

TEST(Valid_BMS, static_assert)
{
    EXPECT_TRUE(test_for_success("valid_bms/static_assert.bms"));
}

TEST(Valid_BMS, void)
{
    EXPECT_TRUE(test_for_success("valid_bms/void.bms"));
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
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_global_const,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/failed_to_define/global_const.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_function,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define/function.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_parameter)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_parameter,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define/parameter.bms", expectations));
}

TEST(BMS_Analysis_Error, failed_to_define_variable)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::failed_to_define_variable,
          .fail_line = 4,
          .cause_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/failed_to_define/variable.bms", expectations));
}

TEST(BMS_Analysis_Error, reference_to_undefined_variable)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::reference_to_undefined_variable, .fail_line = 3 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/reference_to_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, assignment_of_undefined_variable)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assignment_of_undefined_variable, .fail_line = 3 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/assignment_of_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, call_to_undefined_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::call_to_undefined_function, .fail_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/call_to_undefined_function.bms", expectations));
}

TEST(BMS_Analysis_Error, width_not_integer)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_not_integer, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_not_integer.bms", expectations));
}

TEST(BMS_Analysis_Error, width_invalid_because_negative)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_invalid, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_invalid/negative.bms", expectations));
}

TEST(BMS_Analysis_Error, width_invalid_because_too_large)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_invalid, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_invalid/too_large.bms", expectations));
}

TEST(BMS_Analysis_Error, width_invalid_because_zero)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_invalid, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/width_invalid/zero.bms", expectations));
}

// FIXME: expected_constant_expression might be dead diagnostic for same reason as above

TEST(BMS_Analysis_Error, let_variable_in_constant_expression)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::let_variable_in_constant_expression,
          .fail_line = 4,
          .cause_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/let_variable_in_constant_expression.bms",
                                    expectations));
}

TEST(BMS_Analysis_Error, parameter_in_constant_expression)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::parameter_in_constant_expression,
          .fail_line = 3,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/parameter_in_constant_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, function_in_expression)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::function_in_expression,
          .fail_line = 4,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/function_in_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, condition_not_bool_in_if_expression)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::condition_not_bool, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/condition_not_bool/if_expression.bms", expectations));
}

TEST(BMS_Analysis_Error, condition_not_bool_in_if_statement)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::condition_not_bool, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/condition_not_bool/if_statement.bms", expectations));
}

TEST(BMS_Analysis_Error, invalid_integer_literal)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::invalid_integer_literal, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/invalid_integer_literal.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_parameter)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_parameter, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning/parameter.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_function, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning/function.bms", expectations));
}

TEST(BMS_Analysis_Error, assigning_const)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::assigning_const, .fail_line = 3, .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/assigning/const.bms", expectations));
}

TEST(BMS_Analysis_Error, call_non_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::call_non_function, .fail_line = 3, .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/call_non_function.bms", expectations));
}

TEST(BMS_Analysis_Error, wrong_number_of_arguments)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::wrong_number_of_arguments,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/wrong_number_of_arguments.bms", expectations));
}

TEST(BMS_Analysis_Error, codegen_call_to_unanalyzed)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::codegen_call_to_unanalyzed,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/codegen_call_to_unanalyzed.bms", expectations));
}

TEST(BMS_Analysis_Error, width_deduction_from_non_uint)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::width_deduction_from_non_uint,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/width_deduction_from_non_uint.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assert_expression_not_bool)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assert_expression_not_bool, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/static_assert_expression_not_bool.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assertion_failed_constant)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assertion_failed, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/static_assertion_failed/constant.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assertion_failed_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assertion_failed, .fail_line = 4 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/static_assertion_failed/function.bms", expectations));
}

TEST(BMS_Analysis_Error, static_assertion_failed_literal)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::static_assertion_failed, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/static_assertion_failed/literal.bms", expectations));
}

TEST(BMS_Analysis_Error, requires_clause_not_bool)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::requires_clause_not_bool,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/requires_clause_not_bool.bms", expectations));
}

TEST(BMS_Analysis_Error, requires_clause_not_satisfied)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::requires_clause_not_satisfied,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/requires_clause_not_satisfied.bms", expectations));
}

TEST(BMS_Analysis_Error, use_of_undefined_variable)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::use_of_undefined_variable,
          .fail_line = 3,
          .cause_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/use_of_undefined_variable.bms", expectations));
}

TEST(BMS_Analysis_Error, use_of_undefined_constant_direct)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::use_of_undefined_constant,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/use_of_undefined_constant/direct.bms", expectations));
}

// TODO: rename and reorder
TEST(BMS_Analysis_Error, use_of_undefined_constant_indirect)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::use_of_undefined_constant,
          .fail_line = 1,
          .cause_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/use_of_undefined_constant/indirect.bms", expectations));
}

TEST(BMS_Analysis_Error, empty_return_in_non_void_function)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::empty_return_in_non_void_function,
          .fail_line = 2,
          .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/empty_return_in_non_void_function.bms", expectations));
}

TEST(BMS_Analysis_Error, void_operation)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::void_operation, .fail_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/void_operation.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_arithmetic)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_arithmetic, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_arithmetic.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_bitwise)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_bitwise, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_bitwise.bms", expectations));
}

TEST(BMS_Analysis_Error, bool_relational_comparison)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::bool_relational_comparison, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/bool_relational_comparison.bms", expectations));
}

TEST(BMS_Analysis_Error, int_bitwise)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::int_bitwise, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/int_bitwise.bms", expectations));
}

TEST(BMS_Analysis_Error, non_bool_logical)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::non_bool_logical, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/non_bool_logical.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_types_in_argument)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_types, .fail_line = 5, .cause_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/incompatible_types/argument.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_types_in_as)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_types, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_types/as.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_types_in_binary_expression)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_types, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_types/binary_expression.bms",
                                    expectations));
}

TEST(BMS_Analysis_Error, incompatible_types_in_return)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_types, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_types/return.bms", expectations));
}

TEST(BMS_Analysis_Error, incompatible_widths)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::incompatible_widths, .fail_line = 3 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/incompatible_widths.bms", expectations));
}

TEST(BMS_Analysis_Error, break_outside_loop)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::break_outside_loop, .fail_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/break_outside_loop.bms", expectations));
}

TEST(BMS_Analysis_Error, continue_outside_loop)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::continue_outside_loop, .fail_line = 2 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/continue_outside_loop.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_unknown)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_unknown, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/annotation/unknown.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_duplicate)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_duplicate, .fail_line = 2, .cause_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/annotation/duplicate.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_not_applicable)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_not_applicable, .fail_line = 1 };
    EXPECT_TRUE(test_for_diagnostic("analysis_error/annotation/not_applicable.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_too_many_arguments)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_too_many_arguments, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/too_many_arguments.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_unknown_parameter)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_unknown_parameter, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/unknown_parameter.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_argument_duplicate)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_argument_duplicate, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/argument_duplicate.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_argument_wrong_type)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_argument_wrong_type, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/argument_wrong_type.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_argument_wrong_value)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_argument_wrong_value, .fail_line = 2 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/argument_wrong_value.bms", expectations));
}

TEST(BMS_Analysis_Error, annotation_missing_argument)
{
    static const Analysis_Error_Expectations expectations //
        { .code = bms::Analysis_Error_Code::annotation_missing_argument, .fail_line = 1 };
    EXPECT_TRUE(
        test_for_diagnostic("analysis_error/annotation/missing_argument.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
