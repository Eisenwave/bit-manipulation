#ifndef BIT_MANIPULATION_BMS_EXECUTION_ERROR_HPP
#define BIT_MANIPULATION_BMS_EXECUTION_ERROR_HPP

#include <optional>

#include "bms/comparison_failure.hpp"
#include "bms/debug_info.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Execution_Error_Code : Default_Underlying {
    load_uninitialized,
    pop,
    pop_call,
    evaluation,
    jump_out_of_program,
    jump_if_not_bool,
    symbolic_jump,
    call_out_of_program,
    infinite_loop,
};

constexpr std::string_view execution_error_code_name(Execution_Error_Code code)
{
    switch (code) {
        using enum Execution_Error_Code;
        BIT_MANIPULATION_ENUM_STRING_CASE(load_uninitialized);
        BIT_MANIPULATION_ENUM_STRING_CASE(pop);
        BIT_MANIPULATION_ENUM_STRING_CASE(pop_call);
        BIT_MANIPULATION_ENUM_STRING_CASE(evaluation);
        BIT_MANIPULATION_ENUM_STRING_CASE(jump_out_of_program);
        BIT_MANIPULATION_ENUM_STRING_CASE(jump_if_not_bool);
        BIT_MANIPULATION_ENUM_STRING_CASE(symbolic_jump);
        BIT_MANIPULATION_ENUM_STRING_CASE(call_out_of_program);
        BIT_MANIPULATION_ENUM_STRING_CASE(infinite_loop);
    };
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

struct Execution_Error {
    /// @brief The error code.
    Execution_Error_Code code;
    /// @brief The evaluation error. Only set to a meaningful value when `code` is `evaluation`.
    Evaluation_Error_Code evaluation_error = {};
    /// @brief Debug info.
    Debug_Info debug_info;
    /// @brief An optional comparison failure.
    /// This would be present when e.g. an `assert(1 == 0)` is executed.
    std::optional<Comparison_Failure> comparison_failure;

    [[nodiscard]] constexpr Execution_Error(Execution_Error_Code code, Debug_Info debug_info)
        : code(code)
        , debug_info(debug_info)
    {
    }

    [[nodiscard]] constexpr Execution_Error(Evaluation_Error_Code code, Debug_Info debug_info)
        : code(Execution_Error_Code::evaluation)
        , evaluation_error(code)
        , debug_info(debug_info)
    {
    }
};

} // namespace bit_manipulation::bms

#endif
