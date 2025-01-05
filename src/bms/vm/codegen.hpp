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
/// @return nothing, or `Analysis_Error` if generation fails
[[nodiscard]] Result<void, Analysis_Error> generate_code(std::pmr::vector<Instruction>& out,
                                                         const ast::Some_Node* function_node,
                                                         const ast::Function& function);

/// @brief Equivalent to `generate_code(out, function_node, get<ast::Function>(*function_node))`.
[[nodiscard]] Result<void, Analysis_Error> generate_code(std::pmr::vector<Instruction>& out,
                                                         const ast::Some_Node* function_node);

enum struct Function_Policy : bool {
    /// @brief Ignore functions that have a valid `vm_address` already.
    ignore,
    /// @brief Always generate VM instructions.
    generate
};

/// @brief Generates code for the whole program.
/// This will process all functions in order of declaration.
/// @param out where instructions are appended
/// @param program the program
/// @param function_policy what to do for functions that already have a `vm_address`
/// @return nothing, or `Analysis_Error` if generation fails
[[nodiscard]] Result<void, Analysis_Error> generate_code(std::pmr::vector<Instruction>& out,
                                                         const ast::Program& program,
                                                         Function_Policy function_policy);

/// @brief Generates code for the whole program and emits the instructions directly into
/// `program.get_vm().instructions()`.
[[nodiscard]] Result<void, Analysis_Error> generate_code(Analyzed_Program& program,
                                                         Function_Policy function_policy);

} // namespace bit_manipulation::bms

#endif
