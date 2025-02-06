#include <string>
#include <string_view>

#include <gtest/gtest.h>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/tty.hpp"

#include "bms/analyzed_program.hpp"

#include "bmd/code_language.hpp"
#include "bmd/codegen/codegen.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

[[nodiscard]] bool gen_code(Code_String& out,
                            std::string_view file,
                            bmd::Code_Language language,
                            std::pmr::memory_resource* memory,
                            const bmd::Code_Options& options)
{
    auto action = [&](const bms::Analyzed_Program& program) -> bool {
        Result<void, bmd::Generator_Error> result
            = bmd::generate_code(out, program, language, memory, options);
        if (!result) {
            Code_String dump { memory };
            print_generator_error(dump, result.error());
            print_code_string(std::cout, dump, should_print_colors);
            return false;
        }
        return true;
    };
    return test_for_success_then_introspect(file, action);
}

[[nodiscard]] bool run_basic_codegen_test(std::string_view file,
                                          bmd::Code_Language language,
                                          std::string_view expected,
                                          const bmd::Code_Options& options)
{
    std::pmr::monotonic_buffer_resource memory;
    Code_String result { &memory };

    const bool success = gen_code(result, file, language, &memory, options);
    if (!success) {
        return false;
    }
    if (result.get_text() == expected) {
        return true;
    }

    const Size separation_breaks = options.compactify ? 2 : 1;

    Code_String dump { &memory };
    dump.append("Test failed because generated code does not match the expected code.\n"
                "Expected:\n",
                Code_Span_Type::diagnostic_error_text);
    dump.append(expected, Code_Span_Type::diagnostic_code_citation);
    dump.append(separation_breaks, '\n');
    dump.append("Actual (syntax highlighting differences are irrelevant):\n",
                Code_Span_Type::diagnostic_error_text);
    print_code_string(std::cout, dump, should_print_colors);

    result.append(separation_breaks, '\n');
    print_code_string(std::cout, result, should_print_colors);

    return false;
}

TEST(Codegen, empty)
{
    constexpr std::string_view file = "codegen/c/empty.bms";
    constexpr std::string_view expected = "";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, void_function)
{
    constexpr std::string_view file = "codegen/c/void_function.bms";
    constexpr std::string_view expected = "void awoo(void){}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, if_statement)
{
    constexpr std::string_view file = "codegen/c/if_statement.bms";
    constexpr std::string_view expected = "void awoo(_Bool x){if(x){int i=0;}else{int j=1;}}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, else_if)
{
    constexpr std::string_view file = "codegen/c/else_if.bms";
    constexpr std::string_view expected
        = "void awoo(void){if(true){}else if(true){}else if(false){}else{}}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, while_loop)
{
    constexpr std::string_view file = "codegen/c/while_loop.bms";
    constexpr std::string_view expected = "void awoo(void){while(true){break;continue;}}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, return_zero)
{
    constexpr std::string_view file = "codegen/c/return_zero.bms";
    constexpr std::string_view expected = "int awoo(void){return 0;}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, min)
{
    constexpr std::string_view file = "codegen/c/min.bms";
    constexpr std::string_view expected = "int min(int x,int y){return y<x?y:x;}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, conversion)
{
    constexpr std::string_view file = "codegen/c/conversion.bms";
    constexpr std::string_view expected = "void awoo(int x){uint32_t y=(uint32_t)x;}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, logic)
{
    constexpr std::string_view file = "codegen/c/logic.bms";
    constexpr std::string_view expected = "_Bool awoo(_Bool x,_Bool y){return!x&&(y||!y);}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

// More rigorous testing of the dependency breaking mechanism can be found elsewhere.
// These tests can be considered integration tests to see if the system works as a whole.

TEST(Codegen, dependency_break_identity_3)
{
    constexpr std::string_view file = "codegen/c/dependency_break/identity_3.bms";
    constexpr std::string_view expected
        = "void chan(void){}void baka(void){chan();}void awoo(void){baka();}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, dependency_break_reverse_3)
{
    constexpr std::string_view file = "codegen/c/dependency_break/reverse_3.bms";
    constexpr std::string_view expected
        = "void chan(void){}void baka(void){chan();}void awoo(void){baka();}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

TEST(Codegen, dependency_break_forward_declaration)
{
    constexpr std::string_view file = "codegen/c/dependency_break/forward_declaration.bms";
    constexpr std::string_view expected = "void chan(void);void baka(void){chan();}void "
                                          "awoo(void){baka();}void chan(void){awoo();}";

    ASSERT_TRUE(
        run_basic_codegen_test(file, bmd::Code_Language::c, expected, { .compactify = true }));
}

} // namespace
} // namespace bit_manipulation
