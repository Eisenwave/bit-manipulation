#ifndef BIT_MANIPULATION_PROGRAM_FILE_TESTING_HPP
#define BIT_MANIPULATION_PROGRAM_FILE_TESTING_HPP

#include <optional>
#include <span>
#include <string_view>

#include "common/function_ref.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

#include "test/diagnostic_policy.hpp"

namespace bit_manipulation {

struct Tokenize_Error_Expectations {
    bms::Tokenize_Error_Code code {};
};

struct Parse_Error_Expectations {
    std::optional<bms::Grammar_Rule> rule {};
    std::optional<Size> line {};
    std::optional<bms::Token_Type> token_type {};
};

/// @brief An expectation towards an error code in a `bms::Analysis_Error`.
/// Besides just checking if the `.code()` matches, this expectation can also
/// verify if the detailed code (e.g. `.type_error()`) matches.
struct Analysis_Error_Code_Expectation {
private:
    bms::Analysis_Error_Code m_code;
    Variant<Monostate, bms::Evaluation_Error_Code, bms::Execution_Error_Code> m_detail;

public:
    Analysis_Error_Code_Expectation(bms::Analysis_Error_Code code)
        : m_code(code)
    {
    }

    Analysis_Error_Code_Expectation(bms::Evaluation_Error_Code code)
        : m_code(bms::Analysis_Error_Code::evaluation_error)
        , m_detail(code)
    {
    }

    Analysis_Error_Code_Expectation(bms::Execution_Error_Code code)
        : m_code(bms::Analysis_Error_Code::execution_error)
        , m_detail(code)
    {
    }

    bool met_by(const bms::Analysis_Error& e) const;

    friend std::ostream& operator<<(std::ostream& out, const Analysis_Error_Code_Expectation& e);
};

struct Analysis_Error_Expectations {
    Analysis_Error_Code_Expectation code;
    std::optional<int> fail_line {};
    std::optional<int> cause_line {};
};

bool test_for_success(std::string_view file, BMS_Stage until_stage = BMS_Stage::analyze);

/// @brief Returns `true` if the program could be analyzed
/// and `introspection(program)` returns `true`.
bool test_for_success_then_introspect(std::string_view file,
                                      Function_Ref<bool(bms::Analyzed_Program&)> introspection);

/// @brief Returns `true` if the program could be analyzed,
/// and also calls `introspection(program)` in that case.
bool test_for_success_also_introspect(std::string_view file,
                                      Function_Ref<void(bms::Analyzed_Program&)> introspection);

bool test_for_diagnostic(std::string_view file, const Tokenize_Error_Expectations& expectations);

bool test_for_diagnostics(std::string_view file,
                          std::span<const Tokenize_Error_Expectations> expectations);

bool test_for_diagnostic(std::string_view file, const Parse_Error_Expectations& expectations);

bool test_for_diagnostic(std::string_view file, const Analysis_Error_Expectations& expectations);

} // namespace bit_manipulation

#endif
