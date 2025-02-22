#include <gtest/gtest.h>

#include "document_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(Valid_BMD, empty)
{
    EXPECT_TRUE(test_for_success("bmd/empty.bmd", BMD_Stage::process));
}

TEST(Valid_BMD, hello_code)
{
    EXPECT_TRUE(test_for_success("bmd/hello_code.bmd", BMD_Stage::process));
}

TEST(Valid_BMD, hello_directive)
{
    EXPECT_TRUE(test_for_success("bmd/hello_directive.bmd", BMD_Stage::process));
}

TEST(Valid_BMD, hello_world)
{
    EXPECT_TRUE(test_for_success("bmd/hello_world.bmd", BMD_Stage::process));
}

} // namespace
} // namespace bit_manipulation
