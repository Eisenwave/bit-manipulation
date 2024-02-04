#ifndef BIT_MANIPULATION_BMS_VM_HPP
#define BIT_MANIPULATION_BMS_VM_HPP

#include <span>
#include <vector>

#include "result.hpp"

#include "bms/linear_map_stack.hpp"
#include "bms/vm_instructions.hpp"

namespace bit_manipulation::bms {

enum struct Execution_Error_Code : int {
    load_uninitialized,
    pop,
    pop_call,
    evaluation,
    jump_out_of_program,
    jump_if_not_bool,
    symbolic_jump,
    call_out_of_program,
};

struct Virtual_Machine {
private:
    std::vector<Instruction> m_instructions;
    Linear_Map_Stack m_function_frame_stack;
    std::vector<Concrete_Value> m_stack;
    Size m_instruction_counter = 0;

public:
    Result<void, Execution_Error_Code> cycle() noexcept;

    void add_instructions(std::span<const Instruction> instructions)
    {
        m_instructions.insert(m_instructions.end(), instructions.begin(), instructions.end());
    }

    void reset() noexcept
    {
        m_function_frame_stack.clear();
        m_stack.clear();
        m_instruction_counter = 0;
    }

    void clear() noexcept
    {
        m_instructions.clear();
        reset();
    }

private:
    template <typename T>
    Result<void, Execution_Error_Code> cycle(T& instruction) = delete;
};

} // namespace bit_manipulation::bms

#endif