#ifndef BIT_MANIPULATION_BMS_VM_CODEGEN_HPP
#define BIT_MANIPULATION_BMS_VM_CODEGEN_HPP

#include "result.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"
#include "bms/vm_instructions.hpp"

namespace bit_manipulation::bms {

Result<void, Analysis_Error>
generate_code(std::vector<Instruction>& out, Parsed_Program& program, ast::Function_Node& function);

}

#endif