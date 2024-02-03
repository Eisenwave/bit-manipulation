#ifndef BIT_MANIPULATION_BMS_VM_CODEGEN_HPP
#define BIT_MANIPULATION_BMS_VM_CODEGEN_HPP

#include "result.hpp"

#include "bms/bms.hpp"
#include "bms/vm.hpp"

namespace bit_manipulation::bms {

Result<std::vector<Instruction>, Analysis_Error> generate_code(Parsed_Program& program,
                                                               ast::Node_Handle function_handle);

}

#endif