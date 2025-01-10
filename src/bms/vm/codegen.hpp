#ifndef BIT_MANIPULATION_BMS_VM_CODEGEN_HPP
#define BIT_MANIPULATION_BMS_VM_CODEGEN_HPP

#include <vector>

#include "common/result.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Call_Policy {
    /// @brief Never resolve call to functions, only emit `Symbolic_Call` instructions.
    always_symbolic,
    /// @brief Resolve calls for which a VM address is conveniently available, but keep
    /// symbolic calls and don't come back in a second pass to resolve those later.
    resolve_if_possible,
    /// @brief Like `resolve_if_possible`, but assert that resolution is always possible.
    assert_resolve_possible,
    /// @brief Emit symbolic calls initially and resolve those to concrete calls in a second pass.
    /// This option is only meaningful when processing multiple functions; otherwise it acts like
    /// `assert_resolve_possible`.
    resolve,
};

/// @brief Generates instructions, appending them to `out` without overwriting any existing results.
void generate_code(std::pmr::vector<Instruction>& out,
                   const ast::Some_Node* function_node,
                   const ast::Function& function,
                   Call_Policy policy);

/// @brief Equivalent to `generate_code(out, function_node, get<ast::Function>(*function_node),
/// policy)`.
void generate_code(std::pmr::vector<Instruction>& out,
                   const ast::Some_Node* function_node,
                   Call_Policy policy);

struct Codegen_Options {
    /// @brief Overwrite the `vm_address` of functions upon generation.
    /// Note that if this function is disabled, functions calling other functions are effectively
    /// banned because the VM address of a function is discarded, so it cannot be referenced later.
    bool write_vm_address = true;
    /// @brief If the function already has a `vm_address`, no codegen is performed.
    bool ignore_with_address = true;
    Call_Policy calls = Call_Policy::assert_resolve_possible;
};

/// @brief Generates code for the whole program.
/// This will process all functions in order of declaration.
void generate_code(std::pmr::vector<Instruction>& out,
                   ast::Program& program,
                   Codegen_Options options = {});

/// @brief Generates code for the whole program and emits the instructions directly into
/// `program.get_vm().instructions()`.
void generate_code(Analyzed_Program& program, Codegen_Options options = {});

} // namespace bit_manipulation::bms

#endif
