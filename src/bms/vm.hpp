#ifndef BIT_MANIPULATION_BMS_VM_HPP
#define BIT_MANIPULATION_BMS_VM_HPP

#include <span>
#include <vector>

#include "result.hpp"

#include "bms/linear_map_stack.hpp"
#include "bms/vm_instructions.hpp"

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
};

struct Virtual_Machine {
private:
    std::vector<Instruction> m_instructions;
    Linear_Map_Stack m_function_frame_stack;
    std::vector<Concrete_Value> m_stack;
    Size m_instruction_counter = 0;

public:
    Result<void, Execution_Error_Code> cycle() noexcept;

    [[nodiscard]] std::vector<Instruction>& instructions() & noexcept
    {
        return m_instructions;
    }

    [[nodiscard]] const std::vector<Instruction>& instructions() const& noexcept
    {
        return m_instructions;
    }

    /// @brief Inserts the instructions into the virtual machine.
    /// @param instructions the instructions
    /// @return the address of the first instruction inserted.
    Size add_instructions(std::span<const Instruction> instructions)
    {
        Size result = m_instructions.size();
        m_instructions.insert(m_instructions.end(), instructions.begin(), instructions.end());
        return result;
    }

    /// @brief Artificially pushes a value onto the stack.
    /// This allows executing functions for a VM in its initial state.
    /// Note that function parameters are expected to be pushed left-to-right, so that the
    /// rightmost parameter ends up being the topmost on the stack.
    /// @param value the value to push onto the stack
    void push(Concrete_Value value)
    {
        m_stack.push_back(value);
    }

    /// @brief Artificially pops a value from the stack.
    /// @return the popped value
    Concrete_Value pop()
    {
        Concrete_Value result = m_stack.back();
        m_stack.pop_back();
        return result;
    }

    /// @brief Artificially sets the instruction counter to the given address.
    /// @param address the address of the next instruction to be executed
    void jump_to(Size address)
    {
        BIT_MANIPULATION_ASSERT(address < m_instructions.size());
        m_instruction_counter = address;
    }

    [[nodiscard]] Size instruction_count() const noexcept
    {
        return m_instructions.size();
    }

    [[nodiscard]] Size stack_size() const noexcept
    {
        return m_stack.size();
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