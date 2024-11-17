#ifndef BIT_MANIPULATION_BMS_EXECUTION_ERROR_HPP
#define BIT_MANIPULATION_BMS_EXECUTION_ERROR_HPP

#include <optional>

#include "bms/comparison_failure.hpp"
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

struct Execution_Error {
    /// @brief The pointer to the AST node which has emitted the failed instruction.
    const ast::Some_Node* handle;
    /// @brief The error code.
    Execution_Error_Code code;
    /// @brief The evaluation error. Only set to a meaningful value when `code` is `evaluation`.
    Evaluation_Error_Code evaluation_error = {};
    /// @brief An optional comparison failure.
    /// This would be present when e.g. an `assert(1 == 0)` is executed.
    std::optional<Comparison_Failure> comparison_failure;

    [[nodiscard]] constexpr Execution_Error(const ast::Some_Node* handle, Execution_Error_Code code)
        : handle(handle)
        , code(code)
    {
    }

    [[nodiscard]] constexpr Execution_Error(const ast::Some_Node* handle,
                                            Evaluation_Error_Code code)
        : handle(handle)
        , code(Execution_Error_Code::evaluation)
        , evaluation_error(code)
    {
    }
};

} // namespace bit_manipulation::bms

#endif
