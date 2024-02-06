#include <unordered_map>
#include <vector>

#include "assert.hpp"

#include "bms/linear_map_stack.hpp"
#include "bms/operations.hpp"
#include "bms/vm.hpp"
#include "bms/vm_instructions.hpp"

namespace bit_manipulation::bms {

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Load& load)
{
    auto pos = m_function_frame_stack.find(load.source);
    if (pos == nullptr) {
        return Execution_Error_Code::load_uninitialized;
    }
    m_stack.push_back(pos->value);
    ++m_instruction_counter;
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Push& push)
{
    m_stack.push_back(push.value);
    ++m_instruction_counter;
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Store& store)
{
    if (m_stack.empty()) {
        return Execution_Error_Code::pop;
    }
    Concrete_Value value = m_stack.back();
    m_stack.pop_back();
    m_function_frame_stack.assign(store.target, value);
    ++m_instruction_counter;
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Relative_Jump& jump)
{
    if (Signed_Size(m_instruction_counter) + jump.offset + 1
        >= Signed_Size(m_instructions.size())) {
        return Execution_Error_Code::jump_out_of_program;
    }
    m_instruction_counter = Size(Signed_Size(m_instruction_counter) + jump.offset + 1);
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Relative_Jump_If& jump_if)
{
    if (m_stack.empty()) {
        return Execution_Error_Code::pop;
    }
    Concrete_Value actual = m_stack.back();
    m_stack.pop_back();
    if (actual.type != Concrete_Type::Bool) {
        return Execution_Error_Code::jump_if_not_bool;
    }
    if (Signed_Size(m_instruction_counter) + jump_if.offset + 1
        >= Signed_Size(m_instructions.size())) {
        return Execution_Error_Code::jump_out_of_program;
    }
    m_instruction_counter
        = Size(Signed_Size(m_instruction_counter + 1)
               + (jump_if.expected == bool(actual.int_value) ? jump_if.offset : 0));
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Return&)
{
    std::optional<Concrete_Value> return_address = m_function_frame_stack.pop_frame();
    if (!return_address) {
        return Execution_Error_Code::pop_call;
    }
    m_instruction_counter = static_cast<Size>(return_address->int_value);
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Unary_Operate& unary_operate)
{
    if (m_stack.empty()) {
        return Execution_Error_Code::pop;
    }
    const Concrete_Value operand = m_stack.back();
    m_stack.pop_back();
    const Result<Concrete_Value, Evaluation_Error_Code> result
        = evaluate_unary_operator(unary_operate.op, operand);
    if (!result) {
        return Execution_Error_Code::evaluation; // TODO better diagnostics
    }
    m_stack.push_back(*result);
    ++m_instruction_counter;
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Binary_Operate& binary_operate)
{
    if (m_stack.size() < 2) {
        return Execution_Error_Code::pop;
    }
    const Concrete_Value rhs = m_stack.back();
    m_stack.pop_back();
    const Concrete_Value lhs = m_stack.back();
    m_stack.pop_back();
    const Result<Concrete_Value, Evaluation_Error_Code> result
        = evaluate_binary_operator(lhs, binary_operate.op, rhs);
    if (!result) {
        return Execution_Error_Code::evaluation; // TODO better diagnostics
    }
    m_stack.push_back(*result);
    ++m_instruction_counter;
    return {};
}

template <>
Result<void, Execution_Error_Code> Virtual_Machine::cycle(ins::Call& call)
{
    if (call.address > m_instructions.size()) {
        return Execution_Error_Code::call_out_of_program;
    }
    const auto return_address = Concrete_Value::Int(Big_Int(m_instruction_counter + 1));
    m_function_frame_stack.push_frame(return_address);
    m_instruction_counter = call.address;
    return {};
}

Result<void, Execution_Error_Code> Virtual_Machine::cycle() noexcept
{
    Instruction next = m_instructions.at(m_instruction_counter);
    return std::visit(
        [this]<typename T>(T& i) -> Result<void, Execution_Error_Code> {
            if constexpr (std::is_same_v<T, ins::Break> || std::is_same_v<T, ins::Continue>) {
                return Execution_Error_Code::symbolic_jump;
            }
            else {
                return cycle(i);
            }
        },
        next);
}

} // namespace bit_manipulation::bms