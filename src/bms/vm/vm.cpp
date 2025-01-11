#include "common/assert.hpp"
#include "common/variant.hpp"

#include "bms/evaluation/builtin_function.hpp"
#include "bms/evaluation/operations.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/linear_map_stack.hpp"
#include "bms/vm/vm.hpp"

namespace bit_manipulation::bms {

static_assert(std::is_trivially_copyable_v<Instruction>);

struct Cycle_Impl {
    Virtual_Machine& self;

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Load& load)
    {
        auto pos = self.m_function_frame_stack.find(load.source);
        if (pos == nullptr) {
            return Execution_Error { Execution_Error_Code::load_uninitialized, load.debug_info };
        }
        self.m_stack.push_back(pos->value);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Store& store)
    {
        if (self.m_stack.empty()) {
            return Execution_Error { Execution_Error_Code::pop, store.debug_info };
        }
        Concrete_Value value = self.m_stack.back();
        self.m_stack.pop_back();
        self.m_function_frame_stack.assign(store.target, value);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Push& push)
    {
        self.m_stack.push_back(push.value);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Pop& pop)
    {
        if (self.m_stack.empty()) {
            return Execution_Error { Execution_Error_Code::pop, pop.debug_info };
        }
        self.m_stack.pop_back();
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Relative_Jump& jump)
    {
        if (Signed_Size(self.m_instruction_counter) + jump.offset + 1
            >= Signed_Size(self.m_instructions.size())) {
            return Execution_Error { Execution_Error_Code::jump_out_of_program, jump.debug_info };
        }
        self.m_instruction_counter
            = Size(Signed_Size(self.m_instruction_counter) + jump.offset + 1);
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Relative_Jump_If& jump_if)
    {
        if (self.m_stack.empty()) {
            return Execution_Error { Execution_Error_Code::pop, jump_if.debug_info };
        }
        Concrete_Value actual = self.m_stack.back();
        self.m_stack.pop_back();
        if (actual.type != Concrete_Type::Bool) {
            return Execution_Error { Execution_Error_Code::jump_if_not_bool, jump_if.debug_info };
        }
        if (Signed_Size(self.m_instruction_counter) + jump_if.offset + 1
            >= Signed_Size(self.m_instructions.size())) {
            return Execution_Error { Execution_Error_Code::jump_out_of_program,
                                     jump_if.debug_info };
        }
        self.m_instruction_counter
            = Size(Signed_Size(self.m_instruction_counter + 1)
                   + (jump_if.expected == bool(actual.int_value) ? jump_if.offset : 0));
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Return& ret)
    {
        std::optional<Concrete_Value> return_address = self.m_function_frame_stack.pop_frame();
        if (!return_address) {
            return Execution_Error { Execution_Error_Code::pop_call, ret.debug_info };
        }
        self.m_instruction_counter = static_cast<Size>(return_address->int_value);
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Convert& convert)
    {
        if (self.m_stack.empty()) {
            return Execution_Error { Execution_Error_Code::pop, convert.debug_info };
        }
        const Concrete_Value operand = self.m_stack.back();
        self.m_stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error_Code> result
            = evaluate_conversion(operand, convert.type);
        if (!result) {
            return Execution_Error { result.error(), convert.debug_info };
        }
        self.m_stack.push_back(*result);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Unary_Operate& unary_operate)
    {
        if (self.m_stack.empty()) {
            return Execution_Error { Execution_Error_Code::pop, unary_operate.debug_info };
        }
        const Concrete_Value operand = self.m_stack.back();
        self.m_stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error_Code> result
            = evaluate_unary_operator(unary_operate.op, operand);
        if (!result) {
            return Execution_Error { result.error(), unary_operate.debug_info };
        }
        self.m_stack.push_back(*result);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error>
    operator()(const ins::Binary_Operate& binary_operate)
    {
        if (self.m_stack.size() < 2) {
            return Execution_Error { Execution_Error_Code::pop, binary_operate.debug_info };
        }
        const Concrete_Value rhs = self.m_stack.back();
        self.m_stack.pop_back();
        const Concrete_Value lhs = self.m_stack.back();
        self.m_stack.pop_back();
        const Result<Concrete_Value, Evaluation_Error_Code> result
            = evaluate_binary_operator(lhs, binary_operate.op, rhs);
        if (!result) {
            return Execution_Error { result.error(), binary_operate.debug_info };
        }
        if (self.m_instruction_counter + 1 < self.m_instructions.size()) {
            const auto* next_builtin_call
                = get_if<ins::Builtin_Call>(&self.m_instructions[self.m_instruction_counter + 1]);
            if (next_builtin_call && next_builtin_call->function == Builtin_Function::assert) {
                self.m_comparison_failure_for_assert
                    = Comparison_Failure { lhs, rhs, binary_operate.op };
            }
        }
        self.m_stack.push_back(*result);
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Call& call)
    {
        if (call.address > self.m_instructions.size()) {
            return Execution_Error { Execution_Error_Code::call_out_of_program, call.debug_info };
        }
        const auto return_address = Concrete_Value::Int(Big_Int(self.m_instruction_counter + 1));
        self.m_function_frame_stack.push_frame(return_address);
        self.m_instruction_counter = call.address;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Builtin_Call& call)
    {
        const Size params = builtin_parameter_count(call.function);
        if (params > self.m_stack.size()) {
            return Execution_Error { Execution_Error_Code::pop, call.debug_info };
        }
        const Result<Concrete_Value, Evaluation_Error_Code> result = evaluate_builtin_function(
            call.function, std::span { self.m_stack.end() - int(params), self.m_stack.end() });
        if (!result) {
            Execution_Error error { result.error(), call.debug_info };
            if (call.function != bms::Builtin_Function::assert
                || !self.m_comparison_failure_for_assert) {
                return error;
            }
            error.comparison_failure = self.m_comparison_failure_for_assert;
            return error;
        }
        self.m_stack.resize(self.m_stack.size() - params);
        if (!result->type.is_monostate()) {
            self.m_stack.push_back(*result);
        }
        ++self.m_instruction_counter;
        return {};
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Symbolic_Call& i)
    {
        return Execution_Error { Execution_Error_Code::symbolic_instruction, i.debug_info };
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Break& i)
    {
        return Execution_Error { Execution_Error_Code::symbolic_instruction, i.debug_info };
    }

    [[nodiscard]] Result<void, Execution_Error> operator()(const ins::Continue& i)
    {
        return Execution_Error { Execution_Error_Code::symbolic_instruction, i.debug_info };
    }
};

[[nodiscard]] Result<void, Execution_Error> Virtual_Machine::cycle()
{
    const auto counter = m_instruction_counter;
    const Instruction& next = m_instructions.at(counter);
    auto result = visit(Cycle_Impl { *this }, next);
    if (!result) {
        return result;
    }
    if (counter == m_instruction_counter) {
        return Execution_Error { Execution_Error_Code::infinite_loop, get_debug_info(next) };
    }
    return result;
}

} // namespace bit_manipulation::bms
