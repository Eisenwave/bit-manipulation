#ifndef BIT_MANIPULATION_PROGRAM_FILE_TESTING_HPP
#define BIT_MANIPULATION_PROGRAM_FILE_TESTING_HPP

#include <optional>
#include <string_view>
#include <variant>

#include "bms/fwd.hpp"

#include "test/diagnostic_policy.hpp"

namespace bit_manipulation {

struct Parse_Error_Expectations {
    std::optional<bms::Grammar_Rule> rule;
    std::optional<Size> line;
    std::optional<bms::Token_Type> token_type;
};

/// @brief An expectation towards an error code in a `bms::Analysis_Error`.
/// Besides just checking if the `.code()` matches, this expectation can also
/// verify if the detailed code (e.g. `.type_error()`) matches.
struct Analysis_Error_Code_Expectation {
private:
    bms::Analysis_Error_Code m_code;
    std::variant<std::monostate,
                 bms::Type_Error_Code,
                 bms::Evaluation_Error_Code,
                 bms::Execution_Error_Code,
                 bms::Conversion_Error_Code>
        m_detail;

public:
    constexpr Analysis_Error_Code_Expectation(bms::Analysis_Error_Code code)
        : m_code(code)
    {
    }

    constexpr Analysis_Error_Code_Expectation(bms::Type_Error_Code code)
        : m_code(bms::Analysis_Error_Code::type_error)
        , m_detail(code)
    {
    }

    constexpr Analysis_Error_Code_Expectation(bms::Evaluation_Error_Code code)
        : m_code(bms::Analysis_Error_Code::evaluation_error)
        , m_detail(code)
    {
    }

    constexpr Analysis_Error_Code_Expectation(bms::Execution_Error_Code code)
        : m_code(bms::Analysis_Error_Code::execution_error)
        , m_detail(code)
    {
    }

    constexpr Analysis_Error_Code_Expectation(bms::Conversion_Error_Code code)
        : m_code(bms::Analysis_Error_Code::conversion_error)
        , m_detail(code)
    {
    }

    bool met_by(const bms::Analysis_Error& e) const;

    friend std::ostream& operator<<(std::ostream& out, const Analysis_Error_Code_Expectation& e);
};

struct Analysis_Error_Expectations {
    Analysis_Error_Code_Expectation code;
    std::optional<int> fail_line;
    std::optional<int> cause_line;
};

bool test_for_success(std::string_view file,
                      Compilation_Stage until_stage = Compilation_Stage::analyze);

bool test_for_diagnostic(std::string_view file, bms::Tokenize_Error_Code expected);

bool test_for_diagnostic(std::string_view file, const Parse_Error_Expectations& expectations);

bool test_for_diagnostic(std::string_view file, const Analysis_Error_Expectations& expectations);

} // namespace bit_manipulation

#endif
