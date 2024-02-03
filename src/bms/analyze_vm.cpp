#include <unordered_map>
#include <vector>

#include "assert.hpp"

#include "bms/linear_map_stack.hpp"
#include "bms/operations.hpp"
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
    unknown_call
};

struct Virtual_Machine {
    std::vector<Instruction> instructions;
    std::unordered_map<ast::Node_Handle, Size> function_addresses;

    Linear_Map_Stack function_frame_stack;
    std::vector<Concrete_Value> stack;
    Size instruction_counter = 0;
    bool halted = false;

public:
    Result<void, Execution_Error_Code> cycle()
    {
        Instruction next = instructions.at(instruction_counter);
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

private:
    template <typename T>
    Result<void, Execution_Error_Code> cycle(T& instruction) = delete;

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Load& load)
    {
        auto pos = function_frame_stack.find(load.source);
        if (pos == nullptr) {
            return Execution_Error_Code::load_uninitialized;
        }
        stack.push_back(pos->value);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Push& push)
    {
        stack.push_back(push.value);
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Store& store)
    {
        if (stack.empty()) {
            return Execution_Error_Code::pop;
        }
        Concrete_Value value = stack.back();
        stack.pop_back();
        function_frame_stack.assign(store.target, value);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Jump& jump)
    {
        if (Signed_Size(instruction_counter) + jump.offset + 1
            >= Signed_Size(instructions.size())) {
            return Execution_Error_Code::jump_out_of_program;
        }
        instruction_counter = Size(Signed_Size(instruction_counter) + jump.offset + 1);
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Jump_If& jump_if)
    {
        if (stack.empty()) {
            return Execution_Error_Code::pop;
        }
        Concrete_Value actual = stack.back();
        stack.pop_back();
        if (actual.type != Concrete_Type::Bool) {
            return Execution_Error_Code::jump_if_not_bool;
        }
        if (Signed_Size(instruction_counter) + jump_if.offset + 1
            >= Signed_Size(instructions.size())) {
            return Execution_Error_Code::jump_out_of_program;
        }
        instruction_counter
            = Size(Signed_Size(instruction_counter + 1)
                   + (jump_if.expected == bool(actual.int_value) ? jump_if.offset : 0));
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Return&)
    {
        std::optional<Concrete_Value> return_address = function_frame_stack.pop_frame();
        if (!return_address) {
            return Execution_Error_Code::pop_call;
        }
        instruction_counter = static_cast<Size>(return_address->int_value);
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Unary_Operate& unary_operate)
    {
        if (stack.empty()) {
            return Execution_Error_Code::pop;
        }
        const Concrete_Value operand = stack.back();
        stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error_Code> result
            = evaluate_unary_operator(unary_operate.op, operand);
        if (!result) {
            return Execution_Error_Code::evaluation; // TODO better diagnostics
        }
        stack.push_back(*result);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Binary_Operate& binary_operate)
    {
        if (stack.size() < 2) {
            return Execution_Error_Code::pop;
        }
        const Concrete_Value rhs = stack.back();
        stack.pop_back();
        const Concrete_Value lhs = stack.back();
        stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error_Code> result
            = evaluate_binary_operator(lhs, binary_operate.op, rhs);
        if (!result) {
            return Execution_Error_Code::evaluation; // TODO better diagnostics
        }
        stack.push_back(*result);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error_Code> cycle(ins::Call& call)
    {
        auto it = function_addresses.find(call.function);
        if (it == function_addresses.end()) {
            return Execution_Error_Code::unknown_call;
        }
        const auto return_address = Concrete_Value::Int(Big_Int(instruction_counter + 1));
        function_frame_stack.push_frame(return_address);
        instruction_counter = it->second;
        return {};
    }
};

} // namespace bit_manipulation::bms