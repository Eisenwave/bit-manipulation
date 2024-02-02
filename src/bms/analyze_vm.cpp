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

    std::optional<Execution_Error> cycle()
    {
        Instruction next = instructions.at(instruction_counter);
        switch (next.type) {

        case Instruction_Type::load: {
            auto pos = objects.find(next.load.source);
            if (pos == objects.end()) {
                return Execution_Error::load_uninitialized;
            }
            stack.push_back(pos->second);
            ++instruction_counter;
            return {};
        }

        case Instruction_Type::push: {
            stack.push_back(next.push.value);
            return {};
        }

        case Instruction_Type::store: {
            if (stack.empty()) {
                return Execution_Error::pop;
            }
            Concrete_Value value = stack.back();
            stack.pop_back();
            auto [it, success] = objects.emplace(next.store.target, value);
            if (!success) {
                it->second = value;
            }
            ++instruction_counter;
            return {};
        }

        case Instruction_Type::jump: {
            if (next.jump.label >= instructions.size()) {
                return Execution_Error::jump_out_of_program;
            }
            instruction_counter = next.jump.label;
            return {};
        }

        case Instruction_Type::jump_if: {
            if (stack.empty()) {
                return Execution_Error::pop;
            }
            Concrete_Value actual = stack.back();
            stack.pop_back();
            if (actual.type != Concrete_Type::Bool) {
                return Execution_Error::jump_if_not_bool;
            }
            if (next.jump_if.label >= instructions.size()) {
                return Execution_Error::jump_out_of_program;
            }
            instruction_counter = next.jump_if.expected == bool(actual.int_value)
                ? next.jump.label
                : instruction_counter + 1;
            return {};
        }

        case Instruction_Type::jump_break:
        case Instruction_Type::jump_continue: {
            return Execution_Error::symbolic_jump;
        }

        case Instruction_Type::ret: {
            if (call_stack.empty()) {
                return Execution_Error::pop_call;
            }

            instruction_counter = call_stack.back();
            call_stack.pop_back();
            return {};
        }

        case Instruction_Type::unary_operate: {
            if (stack.empty()) {
                return Execution_Error::pop;
            }
            Concrete_Value operand = stack.back();
            stack.pop_back();
            Concrete_Evaluation_Result result
                = evaluate_unary_operator(next.unary_operate.op, operand);
            if (!result) {
                return Execution_Error::evaluation; // TODO better diagnostics
            }
            stack.push_back(*result);
            ++instruction_counter;
            return {};
        }

        case Instruction_Type::binary_operate: {
            if (stack.size() < 2) {
                return Execution_Error::pop;
            }
            Concrete_Value rhs = stack.back();
            stack.pop_back();
            Concrete_Value lhs = stack.back();
            stack.pop_back();
            Concrete_Evaluation_Result result
                = evaluate_binary_operator(lhs, next.binary_operate.op, rhs);
            if (!result) {
                return Execution_Error::evaluation; // TODO better diagnostics
            }
            stack.push_back(*result);
            ++instruction_counter;
            return {};
        }

        case Instruction_Type::call: {
            auto it = function_addresses.find(next.call.function);
            if (it == function_addresses.end()) {
                return Execution_Error::unknown_call;
            }
            call_stack.push_back(instruction_counter + 1);
            instruction_counter = it->second;
            return {};
        }
        }
    }
};

} // namespace bit_manipulation::bms