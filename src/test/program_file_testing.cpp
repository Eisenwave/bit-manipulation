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
            if constexpr (std::is_same_v<T, Monostate>) {
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
        return e.evaluation_error() == get<bms::Evaluation_Error_Code>(m_detail);
    case bms::Analysis_Error_Code::execution_error:
        return e.execution_error() == get<bms::Execution_Error_Code>(m_detail);
    default: return true;
    }
}

namespace {

[[nodiscard]] bms::Error_Reaction error_reaction_of_policy_action(Policy_Action action)
{
    using enum Policy_Action;
    switch (action) {
    case CONTINUE: return bms::Error_Reaction::keep_going;
    case SUCCESS:
    case FAILURE: return bms::Error_Reaction::abort;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid policy action.");
}

struct To_Policy_Consumer final : bms::Diagnostic_Consumer {
private:
    BMS_Diagnostic_Policy& m_policy;
    Size m_error_count = 0;
    Policy_Action m_latest_policy_action = Policy_Action::CONTINUE;

public:
    explicit To_Policy_Consumer(BMS_Diagnostic_Policy& policy)
        : m_policy(policy)
    {
    }

    [[nodiscard]] bms::Error_Reaction operator()(bms::Tokenize_Error&& e) final
    {
        return error(std::move(e));
    }

    [[nodiscard]] bms::Error_Reaction operator()(bms::Parse_Error&& e) final
    {
        return error(std::move(e));
    }

    [[nodiscard]] bms::Error_Reaction operator()(bms::Analysis_Error&& e) final
    {
        return error(std::move(e));
    }

    [[nodiscard]] Size error_count() const noexcept
    {
        return m_error_count;
    }

    [[nodiscard]] Policy_Action latest_policy_action() const noexcept
    {
        return m_latest_policy_action;
    }

    void clear() noexcept
    {
        m_error_count = 0;
    }

private:
    [[nodiscard]] bms::Error_Reaction error(auto&& e)
    {
        m_latest_policy_action = m_policy.error(std::move(e));
        return error_reaction_of_policy_action(m_latest_policy_action);
    }
};

/// @brief A `BMS_Diagnostic_Policy` which stores extra `file` and `source` members to enable
/// printing errors.
struct Printing_Diagnostic_Policy : BMS_Diagnostic_Policy {
    std::string_view file;
    std::string_view source;
    const bms::Parsed_Program* parsed_program = nullptr;
};

/// @brief This policy has succeeded when all compilation stages pass without errors.
/// It has failed when any error is raised.
struct Expect_Success_Diagnostic_Policy final : Printing_Diagnostic_Policy {
private:
    bool m_failed = false;
    BMS_Stage m_max_stage = BMS_Stage::analyze;

public:
    explicit Expect_Success_Diagnostic_Policy(BMS_Stage max_stage)
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
        Code_String out;
        print_io_error(out, file, e);
        print_code_string(std::cout, out, should_print_colors);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        m_failed = true;
        Code_String out;
        print_tokenize_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error& e) final
    {
        m_failed = true;
        Code_String out;
        print_parse_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Analysis_Error& e) final
    {
        m_failed = true;
        if (parsed_program != nullptr) {
            Code_String out;
            print_analysis_error(out, *parsed_program, e);
            print_code_string(std::cout, out, should_print_colors);
        }
        return Policy_Action::FAILURE;
    }
    Policy_Action done(BMS_Stage stage)
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
    std::span<const Tokenize_Error_Expectations> m_expectations;
    Size m_index = 0;

public:
    explicit Expect_Tokenize_Error_Diagnostic_Policy(
        std::span<const Tokenize_Error_Expectations> expectations)
        : m_expectations(expectations)
    {
    }

    bool is_success() const
    {
        return m_state == Policy_Action::SUCCESS;
    }

    Policy_Action error(IO_Error_Code e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        Code_String out;
        print_io_error(out, file, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }

    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        Code_String out;
        if (m_index >= m_expectations.size()) {
            out.build(Code_Span_Type::diagnostic_error_text)
                .append("Too many errors! Expected amount of ")
                .append_integer(m_expectations.size())
                .append(" was exceeded by this error:\n");
        }
        else if (e.code == m_expectations[m_index].code) {
            ++m_index;
            return m_state = (m_index == m_expectations.size() ? Policy_Action::SUCCESS
                                                               : Policy_Action::CONTINUE);
        }
        else {
            out.build(Code_Span_Type::diagnostic_error_text)
                .append("Expected '")
                .append(name_of(m_expectations[m_index].code))
                .append("' but got '")
                .append(name_of(e.code))
                .append("':\n");
        }
        print_tokenize_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
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

    Policy_Action done(BMS_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= BMS_Stage::tokenize);
        if (stage < BMS_Stage::tokenize) {
            return Policy_Action::CONTINUE;
        }
        if (m_index < m_expectations.size()) {
            Code_String out;
            out.build(Code_Span_Type::diagnostic_error_text)
                .append("Not enough errors! Expected ")
                .append_integer(m_expectations.size())
                .append(" errors, but only got ")
                .append_integer(m_index)
                .append("\n");
            print_code_string(std::cout, out, should_print_colors);
            return m_state = Policy_Action::FAILURE;
        }
        return m_state = Policy_Action::SUCCESS;
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
        Code_String out;
        print_io_error(out, file, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        Code_String out;
        print_tokenize_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);

        const auto test_expectations = [&]() -> bool {
            if (m_expectations.rule && e.fail_rule != *m_expectations.rule) {
                std::cout << color(ansi::red) << "Expected error while matching '"
                          << grammar_rule_name(*m_expectations.rule) //
                          << "' but got '" << grammar_rule_name(e.fail_rule) << "':\n";
                return false;
            }
            if (m_expectations.line && e.fail_token.pos.line != *m_expectations.line - 1) {
                std::cout << color(ansi::red) << "Expected parse error on line "
                          << *m_expectations.line //
                          << " but error was on line " << e.fail_token.pos.line + 1 << ":\n";
                return false;
            }
            if (m_expectations.token_type && e.fail_token.type != *m_expectations.token_type) {
                std::cout << color(ansi::red) << "Expected parse error at token of type "
                          << token_type_readable_name(*m_expectations.token_type) //
                          << " but error was at " << token_type_readable_name(e.fail_token.type)
                          << ":\n";
                return false;
            }
            return true;
        };

        if (!test_expectations()) {
            Code_String out;
            print_parse_error(out, file, source, e);
            print_code_string(std::cout, out, should_print_colors);
            return m_state = Policy_Action::FAILURE;
        }

        return m_state = Policy_Action::SUCCESS;
    }
    Policy_Action error(const bms::Analysis_Error&) final
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE();
    }
    Policy_Action done(BMS_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        BIT_MANIPULATION_ASSERT(stage <= BMS_Stage::parse);
        return stage == BMS_Stage::parse ? m_state = Policy_Action::FAILURE
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
        Code_String out;
        print_io_error(out, file, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Tokenize_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        Code_String out;
        print_tokenize_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }
    Policy_Action error(const bms::Parse_Error& e) final
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        Code_String out;
        print_parse_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_state = Policy_Action::FAILURE;
    }

    Policy_Action error(const bms::Analysis_Error& e) final
    {
        const auto test_expectations = [&]() -> bool {
            if (!m_expectations.code.met_by(e)) {
                std::cout << color(ansi::red) << "Expected '" << m_expectations.code //
                          << "' but got '";
                print_analysis_error_name(std::cout, e) << "':\n";
                return false;
            }
            if (m_expectations.fail_line) {
                auto pos = e.fail_pos();
                BIT_MANIPULATION_ASSERT(pos);
                if (pos->line != Size(*m_expectations.fail_line - 1)) {
                    std::cout << color(ansi::red) << "Expected analysis error on line "
                              << *m_expectations.fail_line //
                              << " but error was on line " << pos->line + 1 << ":\n";
                    return false;
                }
            }
            if (m_expectations.cause_line) {
                if (!e.cause()) {
                    std::cout << color(ansi::red)
                              << "Expected analysis error to have cause on line "
                              << *m_expectations.cause_line //
                              << " but error has no cause:\n";
                    return false;
                }
                auto pos = e.cause_pos();
                BIT_MANIPULATION_ASSERT(pos);
                if (pos->line != Size(*m_expectations.cause_line - 1)) {
                    std::cout << color(ansi::red) << "Expected analysis error cause on line "
                              << *m_expectations.cause_line //
                              << " but cause was on line " << pos->line + 1 << ":\n";
                    return false;
                }
            }
            return true;
        };

        if (!test_expectations()) {
            BIT_MANIPULATION_ASSERT(parsed_program);
            Code_String out;
            print_analysis_error(out, *parsed_program, e);
            print_code_string(std::cout, out, should_print_colors);
            return m_state = Policy_Action::FAILURE;
        }
        return m_state = Policy_Action::SUCCESS;
    }

    Policy_Action done(BMS_Stage stage)
    {
        BIT_MANIPULATION_ASSERT(m_state == Policy_Action::CONTINUE);
        if (stage == BMS_Stage::analyze) {
            std::cout << color(ansi::red) << "Expected '" << m_expectations.code //
                      << "' but program was analyzed with no errors.\n"
                      << color(ansi::reset);
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
                   Printing_Diagnostic_Policy& policy,
                   std::function<bool(const bms::Analyzed_Program&)> introspect = {})
{
#define BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(...)                                              \
    switch (__VA_ARGS__) {                                                                         \
    case Policy_Action::SUCCESS: return true;                                                      \
    case Policy_Action::FAILURE: return false;                                                     \
    case Policy_Action::CONTINUE: break;                                                           \
    }

    const auto full_path = "test/" + std::string(file);
    policy.file = full_path;

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::vector<char>, IO_Error_Code> source_data = file_to_bytes(full_path, &memory);
    if (!source_data) {
        return policy.error(source_data.error()) == Policy_Action::SUCCESS;
    }
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMS_Stage::load_file));
    const std::string_view source { source_data->data(), source_data->size() };
    policy.source = source;

    std::pmr::vector<bms::Token> tokens(&memory);
    To_Policy_Consumer diagnostic_consumer { policy };
    bms::tokenize(tokens, source, diagnostic_consumer);
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(diagnostic_consumer.latest_policy_action());
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMS_Stage::tokenize));

    bms::Parsed_Program parsed { source, &memory };
    policy.parsed_program = &parsed;
    bms::parse(parsed, tokens, diagnostic_consumer);
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(diagnostic_consumer.latest_policy_action());
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMS_Stage::parse));

    bms::Analyzed_Program analyzed(parsed, full_path, &memory);
    bms::analyze(analyzed, parsed, &memory, diagnostic_consumer);
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(diagnostic_consumer.latest_policy_action());
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMS_Stage::analyze));

    if (!policy.is_success()) {
        return false;
    }
    if (introspect && !introspect(analyzed)) {
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

bool test_for_success_then_introspect(
    std::string_view file,
    std::function<bool(const bms::Analyzed_Program&)> introspection)
{
    Expect_Success_Diagnostic_Policy policy { BMS_Stage::introspect };
    return test_validity(file, policy, std::move(introspection));
}

bool test_for_diagnostics(std::string_view file,
                          std::span<const Tokenize_Error_Expectations> expectations)
{
    Expect_Tokenize_Error_Diagnostic_Policy policy { expectations };
    return test_validity(file, policy);
}

bool test_for_diagnostic(std::string_view file, const Tokenize_Error_Expectations& expectations)
{
    Expect_Tokenize_Error_Diagnostic_Policy policy { std::span { &expectations, 1 } };
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
