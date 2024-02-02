#include <unordered_map>
#include <vector>

#include "assert.hpp"

#include "bms/operations.hpp"
#include "bms/vm.hpp"

namespace bit_manipulation::bms {

enum struct Execution_Error {
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

    std::unordered_map<ast::Node_Handle, Concrete_Value> objects;
    std::vector<Concrete_Value> stack;
    std::vector<Size> call_stack;
    Size instruction_counter = 0;
    bool halted = false;

public:
    Result<void, Execution_Error> cycle()
    {
        Instruction next = instructions.at(instruction_counter);
        return std::visit(
            [this]<typename T>(T& i) {
                if constexpr (std::is_same_v<T, ins::Break> || std::is_same_v<T, ins::Continue>) {
                    return Execution_Error::symbolic_jump;
                }
                else {
                    return cycle(i);
                }
            },
            instructions.at(instruction_counter));
    }

private:
    template <typename T>
    Result<void, Execution_Error> cycle(T& instruction) = delete;

    template <>
    Result<void, Execution_Error> cycle(ins::Load& load)
    {
        auto pos = objects.find(load.source);
        if (pos == objects.end()) {
            return Execution_Error::load_uninitialized;
        }
        stack.push_back(pos->second);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Push& push)
    {
        stack.push_back(push.value);
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Store& store)
    {
        if (stack.empty()) {
            return Execution_Error::pop;
        }
        Concrete_Value value = stack.back();
        stack.pop_back();
        auto [it, success] = objects.emplace(store.target, value);
        if (!success) {
            it->second = value;
        }
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Jump& jump)
    {
        if (instruction_counter + jump.offset + 1 >= instructions.size()) {
            return Execution_Error::jump_out_of_program;
        }
        instruction_counter = instruction_counter + jump.offset + 1;
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Jump_If& jump_if)
    {
        if (stack.empty()) {
            return Execution_Error::pop;
        }
        Concrete_Value actual = stack.back();
        stack.pop_back();
        if (actual.type != Concrete_Type::Bool) {
            return Execution_Error::jump_if_not_bool;
        }
        if (instruction_counter + jump_if.offset + 1 >= instructions.size()) {
            return Execution_Error::jump_out_of_program;
        }
        instruction_counter = instruction_counter + 1
            + (jump_if.expected == bool(actual.int_value) ? jump_if.offset : 0);
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Return& ret)
    {
        if (call_stack.empty()) {
            return Execution_Error::pop_call;
        }

        instruction_counter = call_stack.back();
        call_stack.pop_back();
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Unary_Operate& unary_operate)
    {
        if (stack.empty()) {
            return Execution_Error::pop;
        }
        const Concrete_Value operand = stack.back();
        stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error> result
            = evaluate_unary_operator(unary_operate.op, operand);
        if (!result) {
            return Execution_Error::evaluation; // TODO better diagnostics
        }
        stack.push_back(*result);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Binary_Operate& binary_operate)
    {
        if (stack.size() < 2) {
            return Execution_Error::pop;
        }
        const Concrete_Value rhs = stack.back();
        stack.pop_back();
        const Concrete_Value lhs = stack.back();
        stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error> result
            = evaluate_binary_operator(lhs, binary_operate.op, rhs);
        if (!result) {
            return Execution_Error::evaluation; // TODO better diagnostics
        }
        stack.push_back(*result);
        ++instruction_counter;
        return {};
    }

    template <>
    Result<void, Execution_Error> cycle(ins::Call& call)
    {
        auto it = function_addresses.find(call.function);
        if (it == function_addresses.end()) {
            return Execution_Error::unknown_call;
        }
        call_stack.push_back(instruction_counter + 1);
        instruction_counter = it->second;
        return {};
    }
};

} // namespace bit_manipulation::bms