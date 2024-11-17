#include <string>

#include <gtest/gtest.h>

#include <common/io.hpp>
#include <common/result.hpp>

#include <bms/analyze.hpp>
#include <bms/parse.hpp>
#include <bms/tokenize.hpp>

namespace bit_manipulation {

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

TEST(SampleTest, is_well_formed)
{
    std::pmr::monotonic_buffer_resource memory;
    std::string_view file = "test/assert.bms";
    std::pmr::string source = load_file(file, &memory);

    bms::Parsed_Program parsed = parse_bms_file(source, file, &memory);
    bms::Analyzed_Program analyzed { parsed, file, &memory };

    Result<void, bms::Analysis_Error> analysis = bms::analyze(analyzed, &memory);
    EXPECT_TRUE(analysis);
}

} // namespace bit_manipulation
