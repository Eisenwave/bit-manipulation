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
#include "bms/expression_type.hpp"
#include "bms/vm/codegen.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/vm.hpp"

#include "test/bms/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

[[nodiscard]] std::optional<std::pmr::vector<bms::Instruction>>
test_and_run_codegen(std::string_view file, std::pmr::memory_resource* memory)
{
    std::optional<std::pmr::vector<bms::Instruction>> result;
    const bool success = test_for_success_also_introspect(
        file,
        [&](bms::Analyzed_Program& program) {
            bms::generate_code(program,
                               { .write_vm_address = true,
                                 .ignore_with_address = true,
                                 .calls = bms::Call_Policy::resolve });
            result = std::move(program.get_vm().instructions());
        },
        memory);
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

constexpr auto push_zero = bms::ins::Push { {}, bms::Concrete_Value::Int(0) };
constexpr auto push_true = bms::ins::Push { {}, bms::Concrete_Value::True };

TEST(VM_Codegen, reverse_order)
{
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

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/reverse_order.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, simple)
{
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

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/simple.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, function_minimal)
{
    static const bms::Instruction expected[] = { bms::ins::Return {} };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/function_minimal.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, const_emits_nothing)
{
    static const bms::Instruction expected[] = {
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual
        = test_and_run_codegen("codegen/vm/const_emits_nothing.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, let_uninitialized)
{
    static const bms::Instruction expected[] = {
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/let_uninitialized.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, let_zero)
{
    static const bms::Instruction expected[] = {
        push_zero,
        bms::ins::Store {},
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/let_zero.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, static_assert_emits_nothing)
{
    static const bms::Instruction expected[] = {
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual
        = test_and_run_codegen("codegen/vm/static_assert_emits_nothing.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, if_else)
{
    // Note that some of the code emitted here is dead.
    // Specifically, the relative jump following return will never be executed.
    // Fixing this would require a more general form of control flow analysis than just
    // return analysis, which would identifies each statement as reachable or not.
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Relative_Jump_If { .offset = 3, .expected = false },
        bms::ins::Push { .value = bms::Concrete_Value::Int(1) },
        bms::ins::Return {},
        bms::ins::Relative_Jump { .offset = 2 },
        push_zero,
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/if_else.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, while_loop)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Relative_Jump_If { .offset = 1, .expected = false },
        bms::ins::Relative_Jump { .offset = -3 },
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/while_loop.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, while_break)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        push_true,
        bms::ins::Relative_Jump_If { .offset = 4, .expected = false }, // while
        bms::ins::Load {},
        bms::ins::Relative_Jump_If { .offset = 1, .expected = false }, // if
        bms::ins::Relative_Jump { .offset = 1 }, // break
        bms::ins::Relative_Jump { .offset = -6 }, // end of while
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/while_break.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, while_continue)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        push_true,
        bms::ins::Relative_Jump_If { .offset = 4, .expected = false }, // while
        bms::ins::Load {},
        bms::ins::Relative_Jump_If { .offset = 1, .expected = false }, // if
        bms::ins::Relative_Jump { .offset = -5 }, // continue
        bms::ins::Relative_Jump { .offset = -6 }, // end of while
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/while_continue.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, returning_constant)
{
    static const bms::Instruction expected[] = {
        bms::ins::Push { {}, bms::Concrete_Value(bms::Concrete_Type::Uint(32), 0) },
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/returning_constant.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, returning_converted)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Convert { {}, bms::Concrete_Type::Uint(32) },
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual
        = test_and_run_codegen("codegen/vm/returning_converted.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, returning_void)
{
    static const bms::Instruction expected[] = {
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/returning_void.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, assignment)
{
    static const bms::Instruction expected[] = {
        push_zero,
        bms::ins::Store {},
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/assignment.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, conversion)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Convert { {}, bms::Concrete_Type::Uint(32) },
        bms::ins::Store {},
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/conversion.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

TEST(VM_Codegen, min)
{
    static const bms::Instruction expected[] = {
        bms::ins::Store {},
        bms::ins::Store {},
        bms::ins::Load {},
        bms::ins::Load {},
        bms::ins::Binary_Operate { .op = bms::Expression_Type::less_than },
        bms::ins::Relative_Jump_If { .offset = 2, .expected = false },
        bms::ins::Load {}, // y
        bms::ins::Relative_Jump { .offset = 1 },
        bms::ins::Load {}, // x
        bms::ins::Return {},
    };

    std::pmr::monotonic_buffer_resource memory;
    const std::optional actual = test_and_run_codegen("codegen/vm/min.bms", &memory);
    EXPECT_TRUE(require_equals_or_dump(expected, actual.value()));
}

} // namespace
} // namespace bit_manipulation
