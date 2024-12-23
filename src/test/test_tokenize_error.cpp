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

TEST(BMS_Tokenize_Error, unterminated_string)
{
    constexpr Tokenize_Error_Expectations expectations //
        { .code = bms::Tokenize_Error_Code::unterminated_string };
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/unterminated_string.bms", expectations));
}

TEST(BMS_Multi_Tokenize_Error, illegal_characters_at_start_of_identifier)
{
    // In this test, we make sure that the initial error of the illegal '$' character does not
    // subsequently produce more errors with tokenizing the identifier.
    constexpr Tokenize_Error_Expectations expectations[]
        = { { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character } };
    EXPECT_TRUE(test_for_diagnostics(
        "tokenize_error/multi_illegal_character/at_start_of_identifier.bms", expectations));
}

TEST(BMS_Multi_Tokenize_Error, illegal_characters_space_separated)
{
    // In this most simple multi-error test, we check that there are exactly as many errors
    // as expected.
    constexpr Tokenize_Error_Expectations expectations[]
        = { { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character } };
    EXPECT_TRUE(test_for_diagnostics("tokenize_error/multi_illegal_character/space_separated.bms",
                                     expectations));
}

TEST(BMS_Multi_Tokenize_Error, illegal_characters_space_separated_recovering)
{
    // In this test, we verify that errors aren't spammed, and some recovery takes place.
    // Namely, if we have multiple illegal characters in a row, this should not produce multiple
    // tokenization errors.
    //
    // New distinct errors are only meant to be produced after whitespace or a token has been
    // matched successfully.
    constexpr Tokenize_Error_Expectations expectations[]
        = { { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character },
            { .code = bms::Tokenize_Error_Code::illegal_character } };
    EXPECT_TRUE(test_for_diagnostics(
        "tokenize_error/multi_illegal_character/space_separated_recovering.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
