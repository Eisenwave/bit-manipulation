#ifndef BIT_MANIPULATION_BMS_VM_HPP
#define BIT_MANIPULATION_BMS_VM_HPP

#include <memory_resource>
#include <span>
#include <vector>

#include "common/result.hpp"
#include "common/variant.hpp"

#include "bms/comparison_failure.hpp"
#include "bms/vm/execution_error.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/linear_map_stack.hpp"

namespace bit_manipulation::bms {

struct Virtual_Machine {
private:
    std::pmr::unsynchronized_pool_resource m_memory_resource;
    std::pmr::vector<Instruction> m_instructions;
    Linear_Map_Stack m_function_frame_stack;
    std::pmr::vector<Concrete_Value> m_stack;
    Size m_instruction_counter = 0;
    std::optional<Comparison_Failure> m_comparison_failure_for_assert;

public:
    explicit Virtual_Machine(std::pmr::memory_resource* memory)
        : m_memory_resource(memory)
        , m_instructions(&m_memory_resource)
        , m_function_frame_stack(&m_memory_resource)
        , m_stack(&m_memory_resource)
    {
    }

    Result<void, Execution_Error> cycle();

    [[nodiscard]] std::pmr::vector<Instruction>& instructions() &
    {
        return m_instructions;
    }

    [[nodiscard]] const std::pmr::vector<Instruction>& instructions() const&
    {
        return m_instructions;
    }

    /// @brief Returns a copy of the next instruction to be executed if `cycle()` was called.
    [[nodiscard]] Instruction next_instruction() const&
    {
        return m_instructions[m_instruction_counter];
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

    [[nodiscard]] Size instruction_count() const
    {
        return m_instructions.size();
    }

    [[nodiscard]] Size stack_size() const
    {
        return m_stack.size();
    }

    void reset()
    {
        m_function_frame_stack.clear();
        m_stack.clear();
        m_instruction_counter = 0;
    }

    void clear()
    {
        m_instructions.clear();
        reset();
    }

private:
    friend struct Cycle_Impl;
};

} // namespace bit_manipulation::bms

#endif
