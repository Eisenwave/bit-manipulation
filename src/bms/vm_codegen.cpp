#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"
#include "bms/vm_instructions.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

namespace {

struct Virtual_Code_Generator : Analyzer_Base {
private:
    std::vector<Instruction>& out;

public:
    Virtual_Code_Generator(Parsed_Program& program, std::vector<Instruction>& out)
        : Analyzer_Base(program)
        , out(out)
    {
    }

public:
    Result<void, Analysis_Error> operator()(Function_Node& function)
    {
        // Functions don't need their own handle for any generation.
        return generate_code(Node_Handle::null, function);
    }

private:
    Result<void, Analysis_Error> generate_code(Node_Handle h)
    {
        return std::visit([this, h](auto& node) { return generate_code(h, node); }, get_node(h));
    }

    template <typename T>
    Result<void, Analysis_Error> generate_code(Node_Handle h, T& node) = delete;

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Program_Node&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("codegen starts at the function level");
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Function_Node& function)
    {
        const auto restore_size = out.size();

        if (function.get_parameters() != Node_Handle::null) {
            auto param_result = generate_code(function.get_parameters());
            if (!param_result) {
                return param_result;
            }
        }

        auto body_result = generate_code(function.get_body());
        if (!body_result) {
            BIT_MANIPULATION_ASSERT(restore_size <= out.size());
            out.resize(restore_size);
            return body_result;
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Parameter_List_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        const auto initial_size = out.size();
        for (Size i = node.parameters.size(); i-- != 0;) {
            // We use left-to-right push order, so storing the parameters in variables upon
            // function entry happens in reverse order.
            auto r = generate_code(node.parameters[i]);
            if (!r) {
                BIT_MANIPULATION_ASSERT(initial_size <= out.size());
                out.resize(initial_size);
                return r;
            }
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle h, Parameter_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        out.push_back(ins::Store { h });
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Type_Node&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "codegen cannot reach type nodes because their parents handle it");
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Const_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        // Const nodes don't produce any codegen because any id expressions that access constants
        // will be constant-folded and emit a `Push` instead.
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle h, Let_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        auto init = generate_code(node.get_initializer());
        if (!init) {
            return init;
        }
        out.push_back(ins::Store { h });
        return init;
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Static_Assert_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, If_Statement_Node& node)
    {
        const auto restore = [this, restore_size = out.size()] {
            BIT_MANIPULATION_ASSERT(restore_size <= out.size());
            out.resize(restore_size);
        };

        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }

        const Size blank_jump_to_else_index = out.size();
        out.push_back(ins::Relative_Jump_If { 0, false });
        const auto size_before_if = out.size();
        auto if_result = generate_code(node.get_if_block());
        if (!if_result) {
            restore();
            return if_result;
        }

        std::get<ins::Relative_Jump_If>(out[blank_jump_to_else_index]).offset = Signed_Size(
            out.size() - size_before_if + (node.get_else_block() != Node_Handle::null));

        if (node.get_else_block() == Node_Handle::null) {
            return {};
        }

        const Size blank_jump_past_else_index = out.size();
        out.push_back(ins::Relative_Jump {});
        const auto size_before_else = out.size();
        auto else_result = generate_code(node.get_else_block());
        if (!else_result) {
            restore();
            return else_result;
        }
        std::get<ins::Relative_Jump>(out[blank_jump_past_else_index]).offset
            = Signed_Size(out.size() - size_before_else);
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, While_Statement_Node& node)
    {
        const auto initial_size = out.size();
        const auto restore = [this, initial_size] {
            BIT_MANIPULATION_ASSERT(initial_size <= out.size());
            out.resize(initial_size);
        };

        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }

        const Size blank_jump_past_loop_index = out.size();
        out.push_back(ins::Relative_Jump_If { 0, false });
        const auto size_before_block = out.size();

        auto block = generate_code(node.get_block());
        if (!block) {
            restore();
            return block;
        }
        std::get<ins::Relative_Jump_If>(out[blank_jump_past_loop_index]).offset
            = Signed_Size(out.size() - size_before_block + 1);

        for (Size i = size_before_block; i < out.size(); ++i) {
            if (std::holds_alternative<ins::Break>(out[i])) {
                const auto past_the_loop = Signed_Size(out.size() - i);
                out[i] = ins::Relative_Jump { past_the_loop };
            }
            if (std::holds_alternative<ins::Continue>(out[i])) {
                const auto to_condition = -Signed_Size(i - initial_size) - 1;
                out[i] = ins::Relative_Jump { to_condition };
            }
        }

        const auto back_offset = -Signed_Size(out.size() - initial_size + 1);
        out.push_back(ins::Relative_Jump { back_offset });
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Jump_Node& node)
    {
        if (node.token.type == Token_Type::keyword_break) {
            out.push_back(ins::Break {});
            return {};
        }
        if (node.token.type == Token_Type::keyword_continue) {
            out.push_back(ins::Continue {});
            return {};
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("jump nodes must only be break or continue");
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Return_Statement_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            out.push_back(ins::Push { node.const_value->concrete_value() });
            out.push_back(ins::Return {});
            return {};
        }

        auto r = generate_code(node.get_expression());
        if (!r) {
            return r;
        }
        out.push_back(ins::Return {});
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Assignment_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        auto result = generate_code(node.get_expression());
        if (!result) {
            return result;
        }
        out.push_back(ins::Store { node.lookup_result });
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Block_Statement_Node& node)
    {
        const auto initial_size = out.size();
        std::vector<Instruction> result;
        for (Node_Handle child : node.get_children()) {
            auto r = generate_code(child);
            if (!r) {
                BIT_MANIPULATION_ASSERT(initial_size <= out.size());
                out.resize(initial_size);
                return r;
            }
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, If_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            out.push_back(ins::Push { node.const_value->concrete_value() });
            return {};
        }
        const auto restore = [this, initial_size = out.size()] {
            BIT_MANIPULATION_ASSERT(initial_size <= out.size());
            out.resize(initial_size);
        };

        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }
        const Size blank_jump_to_right_index = out.size();
        out.push_back(ins::Relative_Jump_If { 0, false });

        const auto size_before_left = out.size();
        auto left = generate_code(node.get_left());
        if (!left) {
            restore();
            return left;
        }
        std::get<ins::Relative_Jump_If>(out[blank_jump_to_right_index]).offset
            = Signed_Size(out.size() - size_before_left + 1);

        const Size blank_jump_past_right_index = out.size();
        out.push_back(ins::Relative_Jump {});
        const auto size_before_right = out.size();
        auto right = generate_code(node.get_right());
        if (!right) {
            restore();
            return right;
        }
        std::get<ins::Relative_Jump>(out[blank_jump_past_right_index]).offset
            = Signed_Size(out.size() - size_before_right);

        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Binary_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            out.push_back(ins::Push { node.const_value->concrete_value() });
            return {};
        }

        const auto restore = [this, restore_size = out.size()] {
            BIT_MANIPULATION_ASSERT(restore_size <= out.size());
            out.resize(restore_size);
        };

        auto left = generate_code(node.get_left());
        if (!left) {
            return left;
        }

        if (node.op == Token_Type::logical_and || node.op == Token_Type::logical_or) {
            /*
            This short-circuiting for || emits something along the lines of:
                left side
                ...
                if (pop() == true) goto short_circuit;
                ...
                right side
                ...
                goto after:
            short_circuit: push(true)
            after: ...
            */
            const bool circuit_breaker = node.op == Token_Type::logical_or;
            const Size blank_jump_to_circuit_break_index = out.size();
            out.push_back(ins::Relative_Jump_If { 0, circuit_breaker });

            const auto size_before_right = out.size();
            auto right = generate_code(node.get_right());
            if (!right) {
                restore();
                return right;
            }
            std::get<ins::Relative_Jump_If>(out[blank_jump_to_circuit_break_index]).offset
                = Signed_Size(out.size() - size_before_right + 1);

            out.push_back(ins::Relative_Jump { 1 });
            out.push_back(ins::Push { Concrete_Value { Concrete_Type::Bool, circuit_breaker } });
        }
        else {
            auto right = generate_code(node.get_right());
            if (!right) {
                restore();
                return right;
            }
            out.push_back(ins::Binary_Operate { node.op });
        }

        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Prefix_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            out.push_back(ins::Push { node.const_value->concrete_value() });
            return {};
        }

        auto init = generate_code(node.get_expression());
        if (!init) {
            return init;
        }
        out.push_back(ins::Unary_Operate { node.op });
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Function_Call_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            out.push_back({ ins::Push { node.const_value->concrete_value() } });
            return {};
        }
        auto& called = std::get<Function_Node>(get_node(node.lookup_result));
        if (!called.const_value || called.vm_address == Function_Node::invalid_vm_address) {
            return Analysis_Error { Analysis_Error_Code::codegen_call_to_unanalyzed, node.token,
                                    called.token };
        }

        const Size restore_size = out.size();
        for (Node_Handle arg : node.arguments) {
            auto arg_code = generate_code(arg);
            if (!arg_code) {
                BIT_MANIPULATION_ASSERT(restore_size <= out.size());
                out.resize(restore_size);
                return arg_code;
            }
        }
        out.push_back(ins::Call { called.vm_address });
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Id_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        auto instruction = node.const_value->int_value
            ? Instruction { ins::Push { node.const_value->concrete_value() } }
            : Instruction { ins::Load { node.lookup_result } };
        out.push_back(instruction);
        return {};
    }

    template <>
    Result<void, Analysis_Error> generate_code(Node_Handle, Literal_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        BIT_MANIPULATION_ASSERT(node.const_value->int_value);

        out.push_back(ins::Push { node.const_value->concrete_value() });
        return {};
    }
};

} // namespace

Result<void, Analysis_Error>
generate_code(std::vector<Instruction>& out, Parsed_Program& program, ast::Function_Node& function)
{
    Virtual_Code_Generator gen { program, out };
    return gen(function);
}

} // namespace bit_manipulation::bms