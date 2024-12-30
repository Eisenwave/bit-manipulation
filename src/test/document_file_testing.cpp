#include <functional>
#include <iostream>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/tty.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "test/diagnostic_policy.hpp"
#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

std::string_view color(std::string_view c)
{
    return should_print_colors ? c : "";
}

bool test_validity(std::string_view file)
{
    const auto full_path = "test/" + std::string(file);

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::vector<char>, IO_Error_Code> source_data = file_to_bytes(full_path, &memory);
    if (!source_data) {
        return false;
    }

    return true;
}

} // namespace

bool test_for_success(std::string_view file, BMS_Stage until_stage)
{
    Expect_Success_Diagnostic_Policy policy { until_stage };
    return test_validity(file, policy);
}

} // namespace bit_manipulation
