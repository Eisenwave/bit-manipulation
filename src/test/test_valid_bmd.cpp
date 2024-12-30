#include <gtest/gtest.h>

#include "document_file_testing.hpp"

namespace bit_manipulation {
namespace {

TEST(Valid_BMD, empty)
{
    EXPECT_TRUE(test_for_success("bmd/empty.bmd", BMD_Stage::process));
}

} // namespace
} // namespace bit_manipulation
