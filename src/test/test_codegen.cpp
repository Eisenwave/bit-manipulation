#include <gtest/gtest.h>

#include "bms/vm/codegen.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {

[[nodiscard]] bool test_and_run_codegen(std::string_view file)
{
    return test_for_success_then_introspect(file, [](bms::Analyzed_Program& program) {
        bms::generate_code(program, { .write_vm_address = true, .ignore_with_address = true });
        return true;
    });
}

TEST(Codegen, reverse_order)
{
    EXPECT_TRUE(test_and_run_codegen("codegen/reverse_order.bms"));
}

TEST(Codegen, simple)
{
    EXPECT_TRUE(test_and_run_codegen("codegen/simple.bms"));
}

} // namespace bit_manipulation
