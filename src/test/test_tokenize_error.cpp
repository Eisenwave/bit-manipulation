#include <gtest/gtest.h>

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(BMS_Tokenize_Error, illegal_character)
{
    constexpr Tokenize_Error_Expectations expectations //
        { .code = bms::Tokenize_Error_Code::illegal_character };
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/illegal_character.bms", expectations));
}

TEST(BMS_Tokenize_Error, no_digits_following_integer_prefix)
{
    constexpr Tokenize_Error_Expectations expectations //
        { .code = bms::Tokenize_Error_Code::no_digits_following_integer_prefix };
    EXPECT_TRUE(
        test_for_diagnostic("tokenize_error/no_digits_following_integer_prefix.bms", expectations));
}

TEST(BMS_Tokenize_Error, integer_suffix)
{
    constexpr Tokenize_Error_Expectations expectations //
        { .code = bms::Tokenize_Error_Code::integer_suffix };
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/integer_suffix.bms", expectations));
}

TEST(BMS_Tokenize_Error, unterminated_comment)
{
    constexpr Tokenize_Error_Expectations expectations //
        { .code = bms::Tokenize_Error_Code::unterminated_comment };
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/unterminated_comment.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
