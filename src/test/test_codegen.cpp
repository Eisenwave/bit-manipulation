#include <algorithm>
#include <iostream>
#include <optional>

#include <gtest/gtest.h>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/tty.hpp"
#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/concrete_value.hpp"
#include "bms/vm/codegen.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/vm.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

[[nodiscard]] std::optional<std::pmr::vector<bms::Instruction>>
test_and_run_codegen(std::string_view file)
{
    std::optional<std::pmr::vector<bms::Instruction>> result;
    const bool success
        = test_for_success_also_introspect(file, [&](bms::Analyzed_Program& program) {
              bms::generate_code(program,
                                 { .write_vm_address = true,
                                   .ignore_with_address = true,
                                   .calls = bms::Call_Policy::resolve });
              result = std::move(program.get_vm().instructions());
          });
    BIT_MANIPULATION_ASSERT(bool(result) == success);
    return result;
}

struct Instructions_Equal {
    [[nodiscard]] bool operator()(const bms::Instruction& x, const bms::Instruction& y) const
    {
        const auto visitor = [&y]<typename T>(const T& xv) {
            if constexpr (one_of<T, bms::ins::Load, bms::ins::Store>) {
                // load and store contain pointers to AST nodes, so at least for now,
                // simply consider any two loads/stores equal for testing purposes
                return true;
            }
            else {
                return xv == get<T>(y);
            }
        };
        return x.index() == y.index() && visit(visitor, x);
    }
};

[[nodiscard]] bool require_equals_or_dump(std::span<const bms::Instruction> expected,
                                          std::span<const bms::Instruction> actual)
{
    static constexpr bms::Program_Print_Options options { .indent = 2, .ignore_debug_info = true };

    if (std::ranges::equal(expected, actual, Instructions_Equal {})) {
        return true;
    }
    Code_String dump;
    dump.append("Codegen test failure! Actual VM instructions don't match expected.\nExpected:\n",
                Code_Span_Type::diagnostic_error_text);
    bms::print_program(dump, expected, options);
    dump.append("\nActual:\n", Code_Span_Type::diagnostic_error_text);
    bms::print_program(dump, actual, options);

    print_code_string(std::cout, dump, should_print_colors);
    return false;
}

TEST(Codegen, reverse_order)
{
    static constexpr auto push_zero = bms::ins::Push { {}, bms::Concrete_Value::Int(0) };
    static const bms::Instruction expected[] = {
        push_zero,
        push_zero,
        bms::ins::Convert { {}, bms::Concrete_Type::Uint(32) },
        bms::ins::Call { {}, 5 },
        bms::ins::Return {},
        bms::ins::Store {},
        bms::ins::Store {},
        push_zero,
        bms::ins::Store {},
        push_zero,
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Store {},
        bms::ins::Return {},
    };

    const std::optional actual = test_and_run_codegen("codegen/vm/reverse_order.bms");

    ASSERT_TRUE(actual);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(Codegen, simple)
{
    static constexpr auto push_zero = bms::ins::Push { {}, bms::Concrete_Value::Int(0) };
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Store {},
        push_zero,
        bms::ins::Store {},
        push_zero,
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Store {},
        bms::ins::Return {},
        push_zero,
        push_zero,
        bms::ins::Convert { {}, bms::Concrete_Type::Uint(32) },
        bms::ins::Call { {}, 0 },
        bms::ins::Return {},
    };

    const std::optional actual = test_and_run_codegen("codegen/vm/simple.bms");

    ASSERT_TRUE(actual);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

} // namespace
} // namespace bit_manipulation
