#include <functional>
#include <iostream>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/tty.hpp"

#include "bmd/html/doc_to_html.hpp"
#include "bmd/parsing/parse.hpp"

#include "test/bms/program_file_testing.hpp"
#include "test/diagnostic_policy.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

struct Printing_BMD_Diagnostic_Policy : BMD_Diagnostic_Policy {
    std::string_view file;
    std::string_view source;
};

bool test_validity(std::string_view file, Printing_BMD_Diagnostic_Policy& policy)
{
#define BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(...)                                              \
    switch (__VA_ARGS__) {                                                                         \
    case Policy_Action::success: return true;                                                      \
    case Policy_Action::failure: return false;                                                     \
    case Policy_Action::keep_going: break;                                                         \
    }

    const auto full_path = "test/" + std::string(file);
    policy.file = full_path;

    std::pmr::monotonic_buffer_resource memory;
    Result<std::pmr::vector<char>, IO_Error_Code> source_data = file_to_bytes(full_path, &memory);
    if (!source_data) {
        return policy.error(source_data.error()) == Policy_Action::success;
    }
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMD_Stage::load_file));
    const std::string_view source { source_data->data(), source_data->size() };
    policy.source = source;

    bmd::Parsed_Document doc = bmd::parse(source, &memory);
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMD_Stage::parse));

// FIXME reimplement
#if 0
    Ignoring_HTML_Token_Consumer consumer;
    Result<void, bmd::Document_Error> result
        = bmd::doc_to_html(consumer, *doc, { .indent_width = 4 }, &memory);
    if (!result) {
        BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.error(result.error()));
    }
    BIT_MANIPULATION_SWITCH_ON_POLICY_ACTION(policy.done(BMD_Stage::process));

    return policy.is_success();
#endif
    return false;
}

struct Expect_Success_Diagnostic_Policy final : Printing_BMD_Diagnostic_Policy {
private:
    Policy_Action m_action = Policy_Action::keep_going;

public:
    virtual bool is_success() const
    {
        return m_action == Policy_Action::success;
    }

    Policy_Action error(IO_Error_Code e) final
    {
        Code_String out;
        print_io_error(out, file, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_action = Policy_Action::failure;
    }

    Policy_Action error(const bmd::Document_Error& e) final
    {
        Code_String out;
        print_document_error(out, file, source, e);
        print_code_string(std::cout, out, should_print_colors);
        return m_action = Policy_Action::failure;
    }

    Policy_Action done(BMD_Stage stage)
    {
        if (stage < BMD_Stage::process) {
            return Policy_Action::keep_going;
        }
        return m_action = Policy_Action::success;
    }
};

} // namespace

bool test_for_success(std::string_view file, BMD_Stage until_stage)
{
    // Sorry, testing for only partial success is not implemented yet.
    BIT_MANIPULATION_ASSERT(until_stage == BMD_Stage::process);

    Expect_Success_Diagnostic_Policy policy;
    return test_validity(file, policy);
}

} // namespace bit_manipulation
