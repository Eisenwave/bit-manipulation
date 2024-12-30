#include <string>

#include <gtest/gtest.h>

#include "common/ansi.hpp"
#include "common/result.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(BMS_Syntax, annotated_everything)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_everything.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, annotated_function_calls)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_function_calls.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, annotated_loop)
{
    EXPECT_TRUE(test_for_success("syntax/annotated_loop.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, annotation_with_arguments)
{
    EXPECT_TRUE(test_for_success("syntax/annotation_with_arguments.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, empty_function_void)
{
    EXPECT_TRUE(test_for_success("syntax/empty_function_void.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, function_return_zero)
{
    EXPECT_TRUE(test_for_success("syntax/function_return_zero.bms", BMS_Stage::parse));
}

TEST(BMS_Syntax, global_const)
{
    EXPECT_TRUE(test_for_success("syntax/global_const.bms", BMS_Stage::parse));
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

TEST(Valid_BMS, empty)
{
    EXPECT_TRUE(test_for_success("valid_bms/empty.bms"));
}

TEST(Valid_BMS, identity)
{
    EXPECT_TRUE(test_for_success("valid_bms/identity.bms"));
}

TEST(Valid_BMS, if_expression)
{
    EXPECT_TRUE(test_for_success("valid_bms/if_expression.bms"));
}

TEST(Valid_BMS, implicit_return_type)
{
    EXPECT_TRUE(test_for_success("valid_bms/implicit_return_type.bms"));
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

TEST(Valid_BMS, uint_pow2)
{
    EXPECT_TRUE(test_for_success("valid_bms/uint_pow2.bms"));
}

TEST(Valid_BMS, uint128)
{
    if (uint_max_width >= 128) {
        EXPECT_TRUE(test_for_success("valid_bms/uint128.bms"));
    }
}

TEST(Valid_BMS, void)
{
    EXPECT_TRUE(test_for_success("valid_bms/void.bms"));
}

} // namespace
} // namespace bit_manipulation
