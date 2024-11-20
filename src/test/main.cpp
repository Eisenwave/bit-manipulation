#include <string>

#include <gtest/gtest.h>

#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/result.hpp"

#include "bms/analyze.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

namespace bit_manipulation {
namespace {

enum struct Compilation_Stage : Default_Underlying { load_file, tokenize, parse, analyze };

constexpr auto operator<=>(Compilation_Stage a, Compilation_Stage b)
{
    return static_cast<Default_Underlying>(a) <=> static_cast<Default_Underlying>(b);
}

std::pmr::string load_file(std::string_view file, std::pmr::memory_resource* memory)
{
    Result<std::pmr::string, IO_Error_Code> result = file_to_string(file, memory);
    BIT_MANIPULATION_ASSERT(result);
    return std::move(*result);
}

std::pmr::vector<bms::Token>
tokenize_bms_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::vector<bms::Token> tokens(memory);
    const Result<void, bms::Tokenize_Error> result = tokenize(tokens, source);
    BIT_MANIPULATION_ASSERT(result);
    return tokens;
}

bms::Parsed_Program
parse_bms_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, memory);
    Result<bms::Parsed_Program, bms::Parse_Error> parsed = bms::parse(tokens, source, memory);
    BIT_MANIPULATION_ASSERT(parsed);
    return std::move(*parsed);
}

/// @brief Returns `true` if the given BMS file passes the various compilation stages.
/// @param file a path to the BMS file, relative to the test assets directory
/// @param diagnostics the diagnostics consumer
/// @param until_stage the (inclusive) maximum stage until which the validity test runs.
/// After that stage is complete, the program is automatically considered valid.
/// For example, `Compilation_Stage::tokenize` would yield `true` if tokenization succeeds.
/// @return `true` if compilation stages have passed
bool test_validity(std::string_view file,
                   bms::Basic_Diagnostic_Consumer& diagnostics,
                   Compilation_Stage until_stage = Compilation_Stage::analyze)
{
    const auto full_path = "test/" + std::string(file);

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::string, IO_Error_Code> source = file_to_string(full_path, &memory);
    if (!source) {
        print_io_error(std::cout, full_path, source.error());
        return false;
    }
    if (until_stage < Compilation_Stage::tokenize) {
        return true;
    }

    std::pmr::vector<bms::Token> tokens(&memory);
    if (!bms::tokenize(tokens, *source, diagnostics)) {
        print_tokenize_error(std::cout, full_path, *source, diagnostics.tokenize_errors.back());
        return false;
    }
    if (until_stage < Compilation_Stage::parse) {
        return true;
    }

    std::optional<bms::Parsed_Program> parsed = bms::parse(tokens, *source, &memory, diagnostics);
    if (!parsed) {
        print_parse_error(std::cout, full_path, *source, diagnostics.parse_errors.back());
        return false;
    }
    if (until_stage < Compilation_Stage::analyze) {
        return true;
    }

    bms::Analyzed_Program analyzed(*parsed, full_path, &memory);
    if (!bms::analyze(analyzed, &memory, diagnostics)) {
        print_analysis_error(std::cout, *parsed, diagnostics.analysis_errors.back());
        return false;
    }

    BIT_MANIPULATION_ASSERT(diagnostics.ok());
    return true;
}

bool test_validity(std::string_view file,
                   Compilation_Stage until_stage = Compilation_Stage::analyze)
{
    bms::Basic_Diagnostic_Consumer diagnostics;
    return test_validity(file, diagnostics, until_stage);
}

TEST(Valid_BMS, assert)
{
    EXPECT_TRUE(test_validity("assert.bms"));
}

TEST(Valid_BMS, deduction)
{
    EXPECT_TRUE(test_validity("deduction.bms"));
}

TEST(Valid_BMS, identity)
{
    EXPECT_TRUE(test_validity("identity.bms"));
}

TEST(Valid_BMS, if_expression)
{
    EXPECT_TRUE(test_validity("if_expression.bms"));
}

TEST(Valid_BMS, loop)
{
    EXPECT_TRUE(test_validity("loop.bms"));
}

TEST(Valid_BMS, static_assert)
{
    EXPECT_TRUE(test_validity("static_assert.bms"));
}

TEST(Valid_BMS, void)
{
    EXPECT_TRUE(test_validity("void.bms"));
}

} // namespace
} // namespace bit_manipulation
