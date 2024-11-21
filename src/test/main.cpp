#include <string>

#include <gtest/gtest.h>

#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/result.hpp"

#include "bms/analyze.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

#include "test/diagnostic_policy.hpp"

namespace bit_manipulation {
namespace {

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

/// @brief A `Diagnostic_Policy` which stores extra `file` and `source` members to enable
/// printing errors.
struct Printing_Diagnostic_Policy : Diagnostic_Policy {
    std::string_view file;
    std::string_view source;
    const bms::Parsed_Program* parsed_program = nullptr;
};

/// @brief A diagnostic policy which expects all compilation stages for a program to succeed.
struct Expect_Success_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    bool m_failed = false;

public:
    Compilation_Stage max_stage = Compilation_Stage::analyze;

    bool is_success() const
    {
        return !m_failed;
    }
    Policy_Action error(IO_Error_Code e) final
    {
        m_failed = true;
        print_io_error(std::cout, file, e);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        m_failed = true;
        print_tokenize_error(std::cout, file, source, e);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error& e) final
    {
        m_failed = true;
        print_parse_error(std::cout, file, source, e);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Analysis_Error& e) final
    {
        m_failed = true;
        if (parsed_program != nullptr) {
            print_analysis_error(std::cout, *parsed_program, e);
        }
        return Policy_Action::FAILURE;
    }
    Policy_Action done(Compilation_Stage stage)
    {
        return m_failed          ? Policy_Action::FAILURE
            : stage >= max_stage ? Policy_Action::SUCCESS
                                 : Policy_Action::CONTINUE;
    }
};

/// @brief Returns `true` if the given BMS file passes the various compilation stages.
/// @param file a path to the BMS file, relative to the test assets directory
/// @param diagnostics the diagnostics consumer
/// @param policy a policy on diagnostics
/// @param until_stage the (inclusive) maximum stage until which the validity test runs.
/// After that stage is complete, the program is automatically considered valid.
/// For example, `Compilation_Stage::tokenize` would yield `true` if tokenization succeeds.
/// @return `true` if compilation stages have passed
bool test_validity(std::string_view file,
                   bms::Basic_Diagnostic_Consumer& diagnostics,
                   Printing_Diagnostic_Policy& policy)
{
    const auto full_path = "test/" + std::string(file);
    policy.file = full_path;

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::string, IO_Error_Code> source = file_to_string(full_path, &memory);
    if (!source) {
        return policy.error(source.error()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Compilation_Stage::load_file)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    }

    std::pmr::vector<bms::Token> tokens(&memory);
    if (!bms::tokenize(tokens, *source, diagnostics)) {
        return policy.error(diagnostics.tokenize_errors.back()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Compilation_Stage::tokenize)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    }

    std::optional<bms::Parsed_Program> parsed = bms::parse(tokens, *source, &memory, diagnostics);
    if (!parsed) {
        return policy.error(diagnostics.parse_errors.back()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Compilation_Stage::parse)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    }

    bms::Analyzed_Program analyzed(*parsed, full_path, &memory);
    if (!bms::analyze(analyzed, &memory, diagnostics)) {
        return policy.error(diagnostics.analysis_errors.back()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Compilation_Stage::analyze)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    }

    BIT_MANIPULATION_ASSERT(diagnostics.ok());
    return policy.is_success();
}

bool test_validity(std::string_view file, Printing_Diagnostic_Policy& policy)
{
    bms::Basic_Diagnostic_Consumer diagnostics;
    return test_validity(file, diagnostics, policy);
}

bool test_for_success(std::string_view file,
                      Compilation_Stage until_stage = Compilation_Stage::analyze)
{
    Expect_Success_Diagnostic_Policy policy;
    policy.max_stage = until_stage;
    return test_validity(file, policy);
}

TEST(Valid_BMS, assert)
{
    EXPECT_TRUE(test_for_success("assert.bms"));
}

TEST(Valid_BMS, deduction)
{
    EXPECT_TRUE(test_for_success("deduction.bms"));
}

TEST(Valid_BMS, identity)
{
    EXPECT_TRUE(test_for_success("identity.bms"));
}

TEST(Valid_BMS, if_expression)
{
    EXPECT_TRUE(test_for_success("if_expression.bms"));
}

TEST(Valid_BMS, loop)
{
    EXPECT_TRUE(test_for_success("loop.bms"));
}

TEST(Valid_BMS, static_assert)
{
    EXPECT_TRUE(test_for_success("static_assert.bms"));
}

TEST(Valid_BMS, void)
{
    EXPECT_TRUE(test_for_success("void.bms"));
}

} // namespace
} // namespace bit_manipulation
