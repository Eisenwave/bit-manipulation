#include <functional>
#include <iostream>

#include "common/diagnostics.hpp"
#include "common/io.hpp"

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/basic_diagnostic_consumer.hpp"
#include "bms/grammar.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

#include "test/diagnostic_policy.hpp"
#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

std::ostream& print_analysis_error_name(std::ostream& out, const bms::Analysis_Error& e)
{
    out << analysis_error_code_name(e.code());

    const auto print_parenthesized
        = [&](std::string_view s) -> std::ostream& { return out << '(' << s << ')'; };

    switch (e.code()) {
        using enum bms::Analysis_Error_Code;
    case execution_error: {
        return print_parenthesized(execution_error_code_name(e.execution_error()));
    }
    case evaluation_error: {
        return print_parenthesized(evaluation_error_code_name(e.evaluation_error()));
    }
    default: return out;
    }
}

} // namespace

std::ostream& operator<<(std::ostream& out, const Analysis_Error_Code_Expectation& e)
{
    out << analysis_error_code_name(e.m_code);

    const auto print_parenthesized
        = [&](std::string_view s) -> std::ostream& { return out << '(' << s << ')'; };

    return visit(
        [&](auto code) -> std::ostream& {
            using T = decltype(code);
            if constexpr (std::is_same_v<T, std::monostate>) {
                return out;
            }
            else if constexpr (std::is_same_v<T, bms::Execution_Error_Code>) {
                return print_parenthesized(execution_error_code_name(code));
            }
            else if constexpr (std::is_same_v<T, bms::Evaluation_Error_Code>) {
                return print_parenthesized(evaluation_error_code_name(code));
            }
            else {
                // TODO: this should be static_assert(false), but we need more recent
                //       IntelliSense to not think that it's bad
                //       The point is that we want this if-chain to be exhaustive.
                BIT_MANIPULATION_ASSERT_UNREACHABLE();
            }
        },
        e.m_detail);
}

bool Analysis_Error_Code_Expectation::met_by(const bms::Analysis_Error& e) const
{
    if (m_code != e.code()) {
        return false;
    }
    switch (m_code) {
    case bms::Analysis_Error_Code::evaluation_error:
        return e.evaluation_error() == std::get<bms::Evaluation_Error_Code>(m_detail);
    case bms::Analysis_Error_Code::execution_error:
        return e.execution_error() == std::get<bms::Execution_Error_Code>(m_detail);
    default: return true;
    }
}

namespace {

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
    Testing_Stage m_max_stage = Testing_Stage::analyze;

public:
    explicit Expect_Success_Diagnostic_Policy(Testing_Stage max_stage)
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
    Policy_Action done(Testing_Stage stage)
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
    Policy_Action done(Testing_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= Testing_Stage::tokenize);
        return stage == Testing_Stage::tokenize ? m_state = Policy_Action::FAILURE
                                                : Policy_Action::CONTINUE;
    }
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

        const auto test_expectations = [&]() -> bool {
            if (m_expectations.rule && e.fail_rule != *m_expectations.rule) {
                std::cout << ansi::red << "Expected error while matching '"
                          << grammar_rule_name(*m_expectations.rule) //
                          << "' but got '" << grammar_rule_name(e.fail_rule) << "':\n";
                return false;
            }
            if (m_expectations.line && e.fail_token.pos.line != *m_expectations.line - 1) {
                std::cout << ansi::red << "Expected parse error on line " << *m_expectations.line //
                          << " but error was on line " << e.fail_token.pos.line + 1 << ":\n";
                return false;
            }
            if (m_expectations.token_type && e.fail_token.type != *m_expectations.token_type) {
                std::cout << ansi::red << "Expected parse error at token of type "
                          << token_type_readable_name(*m_expectations.token_type) //
                          << " but error was at " << token_type_readable_name(e.fail_token.type)
                          << ":\n";
                return false;
            }
            return true;
        };

        if (!test_expectations()) {
            print_parse_error(std::cout, file, source, e);
            return m_state = Policy_Action::FAILURE;
        }

        return m_state = Policy_Action::SUCCESS;
    }
    Policy_Action error(const bms::Analysis_Error&) final
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE();
    }
    Policy_Action done(Testing_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= Testing_Stage::parse);
        return stage == Testing_Stage::parse ? m_state = Policy_Action::FAILURE
                                             : Policy_Action::CONTINUE;
    }
};

/// @brief This policy has succeeded when the expected error is raised during parsing.
/// It has failed when another error is raised, or if parsing succeeds.
struct Expect_Analysis_Error_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    Policy_Action m_state = Policy_Action::CONTINUE;
    Analysis_Error_Expectations m_expectations;

public:
    explicit Expect_Analysis_Error_Diagnostic_Policy(
        const Analysis_Error_Expectations& expectations)
        : m_expectations(expectations)
    {
        BIT_MANIPULATION_ASSERT(!expectations.fail_line || *expectations.fail_line > 0);
        BIT_MANIPULATION_ASSERT(!expectations.cause_line || *expectations.cause_line > 0);
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
        print_parse_error(std::cout, file, source, e);
        return m_state = Policy_Action::FAILURE;
    }

    Policy_Action error(const bms::Analysis_Error& e) final
    {
        const auto test_expectations = [&]() -> bool {
            if (!m_expectations.code.met_by(e)) {
                std::cout << ansi::red << "Expected '" << m_expectations.code //
                          << "' but got '";
                print_analysis_error_name(std::cout, e) << "':\n";
                return false;
            }
            if (m_expectations.fail_line) {
                if (!e.fail) {
                    std::cout << ansi::red << "Expected analysis have failed on line "
                              << *m_expectations.fail_line //
                              << " but it has not failed on any specific AST node:\n";
                    return false;
                }
                std::optional<bit_manipulation::Source_Span> pos = get_source_position(*e.fail);
                BIT_MANIPULATION_ASSERT(pos);
                if (pos->line != Size(*m_expectations.fail_line - 1)) {
                    std::cout << ansi::red << "Expected analysis error on line "
                              << *m_expectations.fail_line //
                              << " but error was on line " << pos->line + 1 << ":\n";
                    return false;
                }
            }
            if (m_expectations.cause_line) {
                if (!e.cause) {
                    std::cout << ansi::red << "Expected analysis error to have cause on line "
                              << *m_expectations.cause_line //
                              << " but error has no cause:\n";
                    return false;
                }
                std::optional<bit_manipulation::Source_Span> pos = get_source_position(*e.cause);
                BIT_MANIPULATION_ASSERT(pos);
                if (pos->line != Size(*m_expectations.cause_line - 1)) {
                    std::cout << ansi::red << "Expected analysis error cause on line "
                              << *m_expectations.cause_line //
                              << " but cause was on line " << pos->line + 1 << ":\n";
                    return false;
                }
            }
            return true;
        };

        if (!test_expectations()) {
            BIT_MANIPULATION_ASSERT(parsed_program);
            print_analysis_error(std::cout, *parsed_program, e);
            return m_state = Policy_Action::FAILURE;
        }
        return m_state = Policy_Action::SUCCESS;
    }

    Policy_Action done(Testing_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        if (stage == Testing_Stage::analyze) {
            std::cout << ansi::red << "Expected '" << m_expectations.code //
                      << "' but program was analyzed with no errors.\n"
                      << ansi::reset;
            return m_state = Policy_Action::FAILURE;
        }
        return Policy_Action::CONTINUE;
    }
};

/// @brief Returns `true` if the given BMS file passes the various compilation stages.
/// @param file a path to the BMS file, relative to the test assets directory
/// @param diagnostics the diagnostics consumer
/// @param policy a policy on diagnostics
/// @param until_stage the (inclusive) maximum stage until which the validity test runs.
/// After that stage is complete, the program is automatically considered valid.
/// For example, `Testing_Stage::tokenize` would yield `true` if tokenization succeeds.
/// @return `true` if compilation stages have passed
bool test_validity(std::string_view file,
                   bms::Basic_Diagnostic_Consumer& diagnostics,
                   Printing_Diagnostic_Policy& policy,
                   std::function<bool(const bms::Analyzed_Program&)> introspect)
{
    const auto full_path = "test/" + std::string(file);
    policy.file = full_path;

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::string, IO_Error_Code> source = file_to_string(full_path, &memory);
    if (!source) {
        return policy.error(source.error()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Testing_Stage::load_file)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    case Policy_Action::CONTINUE: break;
    }

    std::pmr::vector<bms::Token> tokens(&memory);
    if (!bms::tokenize(tokens, *source, diagnostics)) {
        return policy.error(diagnostics.tokenize_errors.back()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Testing_Stage::tokenize)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    case Policy_Action::CONTINUE: break;
    }

    std::optional<bms::Parsed_Program> parsed = bms::parse(tokens, *source, &memory, diagnostics);
    if (!parsed) {
        return policy.error(diagnostics.parse_errors.back()) == Policy_Action::SUCCESS;
    }
    policy.parsed_program = &*parsed;
    switch (policy.done(Testing_Stage::parse)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    case Policy_Action::CONTINUE: break;
    }

    bms::Analyzed_Program analyzed(*parsed, full_path, &memory);
    if (!bms::analyze(analyzed, &memory, diagnostics)) {
        return policy.error(diagnostics.analysis_errors.back()) == Policy_Action::SUCCESS;
    }
    switch (policy.done(Testing_Stage::analyze)) {
    case Policy_Action::SUCCESS: return true;
    case Policy_Action::FAILURE: return false;
    case Policy_Action::CONTINUE: break;
    }

    BIT_MANIPULATION_ASSERT(diagnostics.ok());
    if (!policy.is_success()) {
        return false;
    }
    if (introspect && !introspect(analyzed)) {
        return false;
    }
    return true;
}

bool test_validity(std::string_view file,
                   Printing_Diagnostic_Policy& policy,
                   std::function<bool(const bms::Analyzed_Program&)> introspection = {})
{
    bms::Basic_Diagnostic_Consumer diagnostics;
    return test_validity(file, diagnostics, policy, std::move(introspection));
}

} // namespace

bool test_for_success(std::string_view file, Testing_Stage until_stage)
{
    Expect_Success_Diagnostic_Policy policy { until_stage };
    return test_validity(file, policy);
}

bool test_for_success_then_introspect(
    std::string_view file,
    std::function<bool(const bms::Analyzed_Program&)> introspection)
{
    bms::Basic_Diagnostic_Consumer diagnostics;
    Expect_Success_Diagnostic_Policy policy { Testing_Stage::introspect };
    return test_validity(file, diagnostics, policy, std::move(introspection));
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

bool test_for_diagnostic(std::string_view file, const Analysis_Error_Expectations& expectations)
{
    Expect_Analysis_Error_Diagnostic_Policy policy { expectations };
    return test_validity(file, policy);
}

} // namespace bit_manipulation
