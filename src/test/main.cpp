#include <string>

#include <gtest/gtest.h>

#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/result.hpp"

#include "bms/analyze.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/grammar.hpp"
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

/// @brief This policy has succeeded when all compilation stages pass without errors.
/// It has failed when any error is raised.
struct Expect_Success_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    bool m_failed = false;
    Compilation_Stage m_max_stage = Compilation_Stage::analyze;

public:
    explicit Expect_Success_Diagnostic_Policy(Compilation_Stage max_stage)
        : m_max_stage(max_stage)
    {
    }

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
        return m_failed            ? Policy_Action::FAILURE
            : stage >= m_max_stage ? Policy_Action::SUCCESS
                                   : Policy_Action::CONTINUE;
    }
};

/// @brief This policy has succeeded when the expected error is raised during tokenization.
/// It has failed when another error is raised, or if tokenization succeeds.
struct Expect_Tokenize_Error_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    Policy_Action m_state = Policy_Action::CONTINUE;
    bms::Tokenize_Error_Code m_expected;

public:
    explicit Expect_Tokenize_Error_Diagnostic_Policy(bms::Tokenize_Error_Code expected)
        : m_expected(expected)
    {
    }

    bool is_success() const
    {
        return m_state == Policy_Action::SUCCESS;
    }
    Policy_Action error(IO_Error_Code e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        print_io_error(std::cout, file, e);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        if (e.code == m_expected) {
            return m_state = Policy_Action::SUCCESS;
        }
        std::cout << ansi::red << "Expected '" << name_of(m_expected) //
                  << "' but got '" << name_of(e.code) << "':\n";
        print_tokenize_error(std::cout, file, source, e);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error&) final
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE();
    }
    Policy_Action error(const bms::Analysis_Error&) final
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE();
    }
    Policy_Action done(Compilation_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= Compilation_Stage::tokenize);
        return stage == Compilation_Stage::tokenize ? m_state = Policy_Action::SUCCESS
                                                    : Policy_Action::CONTINUE;
    }
};

struct Parse_Error_Expectations {
    std::optional<bms::Grammar_Rule> rule;
    std::optional<Size> line;
    std::optional<bms::Token_Type> token_type;
};

/// @brief This policy has succeeded when the expected error is raised during parsing.
/// It has failed when another error is raised, or if parsing succeeds.
struct Expect_Parse_Error_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    Policy_Action m_state = Policy_Action::CONTINUE;
    Parse_Error_Expectations m_expectations;

public:
    explicit Expect_Parse_Error_Diagnostic_Policy(const Parse_Error_Expectations& expectations)
        : m_expectations(expectations)
    {
        BIT_MANIPULATION_ASSERT(!expectations.line || *expectations.line > 0);
    }

    bool is_success() const
    {
        return m_state == Policy_Action::SUCCESS;
    }
    Policy_Action error(IO_Error_Code e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        print_io_error(std::cout, file, e);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        print_tokenize_error(std::cout, file, source, e);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        if (m_expectations.rule && e.fail_rule != *m_expectations.rule) {
            std::cout << ansi::red << "Expected error while matching '"
                      << grammar_rule_name(*m_expectations.rule) //
                      << "' but got '" << grammar_rule_name(e.fail_rule) << "':\n";
            goto failed;
        }
        if (m_expectations.line && e.fail_token.pos.line != *m_expectations.line - 1) {
            std::cout << ansi::red << "Expected parse error on line " << *m_expectations.line //
                      << " but error was on line " << e.fail_token.pos.line + 1 << ":\n";
            goto failed;
        }
        if (m_expectations.token_type && e.fail_token.type != *m_expectations.token_type) {
            std::cout << ansi::red << "Expected parse error at token of type "
                      << token_type_readable_name(*m_expectations.token_type) //
                      << " but error was at " << token_type_readable_name(e.fail_token.type)
                      << ":\n";
            goto failed;
        }

        return m_state = Policy_Action::SUCCESS;

    failed:
        print_parse_error(std::cout, file, source, e);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Analysis_Error&) final
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE();
    }
    Policy_Action done(Compilation_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= Compilation_Stage::parse);
        return stage == Compilation_Stage::parse ? m_state = Policy_Action::SUCCESS
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
    Expect_Success_Diagnostic_Policy policy { until_stage };
    return test_validity(file, policy);
}

bool test_for_diagnostic(std::string_view file, bms::Tokenize_Error_Code expected)
{
    Expect_Tokenize_Error_Diagnostic_Policy policy { expected };
    return test_validity(file, policy);
}

bool test_for_diagnostic(std::string_view file, const Parse_Error_Expectations& expectations)
{
    Expect_Parse_Error_Diagnostic_Policy policy { expectations };
    return test_validity(file, policy);
}

TEST(BMS_Syntax, empty_function_void)
{
    EXPECT_TRUE(test_for_success("syntax/empty_function_void.bms"));
}

TEST(BMS_Syntax, function_return_zero)
{
    EXPECT_TRUE(test_for_success("syntax/function_return_zero.bms"));
}

TEST(BMS_Syntax, global_const)
{
    EXPECT_TRUE(test_for_success("syntax/global_const.bms"));
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

TEST(BMS_Tokenize_Error, illegal_character)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/illegal_character.bms",
                                    bms::Tokenize_Error_Code::illegal_character));
}

TEST(BMS_Tokenize_Error, no_digits_following_integer_prefix)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/no_digits_following_integer_prefix.bms",
                                    bms::Tokenize_Error_Code::no_digits_following_integer_prefix));
}

TEST(BMS_Tokenize_Error, integer_suffix)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/integer_suffix.bms",
                                    bms::Tokenize_Error_Code::integer_suffix));
}

TEST(BMS_Tokenize_Error, unterminated_comment)
{
    EXPECT_TRUE(test_for_diagnostic("tokenize_error/unterminated_comment.bms",
                                    bms::Tokenize_Error_Code::unterminated_comment));
}

TEST(BMS_Parse_Error, global_let)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 1, .token_type = bms::Token_Type::keyword_let };
    EXPECT_TRUE(test_for_diagnostic("parse_error/global_let.bms", expectations));
}

TEST(BMS_Parse_Error, let_let)
{
    constexpr Parse_Error_Expectations expectations //
        { .rule = bms::Grammar_Rule::let_declaration,
          .line = 2,
          .token_type = bms::Token_Type::keyword_let };
    EXPECT_TRUE(test_for_diagnostic("parse_error/let_let.bms", expectations));
}

TEST(BMS_Parse_Error, let_no_type_no_init)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 2, .token_type = bms::Token_Type::semicolon };
    EXPECT_TRUE(test_for_diagnostic("parse_error/let_no_type_no_init.bms", expectations));
}

TEST(BMS_Parse_Error, nameless_function)
{
    constexpr Parse_Error_Expectations expectations //
        { .rule = bms::Grammar_Rule::function_declaration,
          .line = 1,
          .token_type = bms::Token_Type::left_parenthesis };
    EXPECT_TRUE(test_for_diagnostic("parse_error/nameless_function.bms", expectations));
}

TEST(BMS_Parse_Error, unbalanced_parentheses)
{
    constexpr Parse_Error_Expectations expectations //
        { .line = 1, .token_type = bms::Token_Type::assign };
    EXPECT_TRUE(test_for_diagnostic("parse_error/unbalanced_parentheses.bms", expectations));
}

} // namespace
} // namespace bit_manipulation
