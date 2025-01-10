#ifndef BIT_MANIPULATION_BMS_VM_CODEGEN_HPP
#define BIT_MANIPULATION_BMS_VM_CODEGEN_HPP

#include <vector>

#include "common/result.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief Generates instructions, appending them to `out` without overwriting any existing results.
/// If generating instructions fails, the contents of `out` remain unchanged.
/// @param out where instructions are appended
/// @param function_node the function node
/// @param function the function itself
void generate_code(std::pmr::vector<Instruction>& out,
                   const ast::Some_Node* function_node,
                   const ast::Function& function);

/// @brief Equivalent to `generate_code(out, function_node, get<ast::Function>(*function_node))`.
void generate_code(std::pmr::vector<Instruction>& out, const ast::Some_Node* function_node);

struct Codegen_Options {
    /// @brief Overwrite the `vm_address` of functions upon generation.
    /// Note that if this function is disabled, functions calling other functions are effectively
    /// banned because the VM address of a function is discarded, so it cannot be referenced later.
    bool write_vm_address = true;
    /// @brief If the function already has a `vm_address`, no codegen is performed.
    bool ignore_with_address = true;
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
