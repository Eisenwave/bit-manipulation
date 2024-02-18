#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"
#include "bms/vm_instructions.hpp"

namespace bit_manipulation::bms {

namespace {

struct Virtual_Code_Generator {
private:
    std::pmr::vector<Instruction>& out;

public:
    Virtual_Code_Generator(std::pmr::vector<Instruction>& out)
        : out(out)
    {
    }

public:
    Result<void, Analysis_Error> operator()(const ast::Function& function)
    {
        // Functions don't need their own handle for any generation.
        return generate_code(nullptr, function);
    }

private:
    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h)
    {
        return fast_visit([this, h](const auto& node) { return generate_code(h, node); }, *h);
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*, const ast::Program&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("codegen starts at the function level");
    }

    // FIXME: full expressions need to generate code which discards unused results

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*, const ast::Function& function)
    {
        const auto restore_size = out.size();

        if (function.get_parameters() != nullptr) {
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

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*,
                                               const ast::Parameter_List& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        const auto initial_size = out.size();
        for (Size i = node.get_children().size(); i-- != 0;) {
            // We use left-to-right push order, so storing the parameters in variables upon
            // function entry happens in reverse order.
            auto r = generate_code(node.get_children()[i]);
            if (!r) {
                BIT_MANIPULATION_ASSERT(initial_size <= out.size());
                out.resize(initial_size);
                return r;
            }
        }
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h, const ast::Parameter& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        out.push_back(ins::Store { { h }, h });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*, const ast::Type&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "codegen cannot reach type nodes because their parents handle it");
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*, const ast::Const& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        // Const nodes don't produce any codegen because any id expressions that access constants
        // will be constant-folded and emit a `Push` instead.
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h, const ast::Let& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        auto init = generate_code(node.get_initializer());
        if (!init) {
            return init;
        }
        out.push_back(ins::Store { { h }, h });
        return init;
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*,
                                               const ast::Static_Assert& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::If_Statement& node)
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
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });
        const auto size_before_if = out.size();
        auto if_result = generate_code(node.get_if_block());
        if (!if_result) {
            restore();
            return if_result;
        }

        std::get<ins::Relative_Jump_If>(out[blank_jump_to_else_index]).offset
            = Signed_Size(out.size() - size_before_if + (node.get_else_block() != nullptr));

        if (node.get_else_block() == nullptr) {
            return {};
        }

        const Size blank_jump_past_else_index = out.size();
        out.push_back(ins::Relative_Jump { { h }, 0 });
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

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::While_Statement& node)
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
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });
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
                out[i] = ins::Relative_Jump { { h }, past_the_loop };
            }
            if (std::holds_alternative<ins::Continue>(out[i])) {
                const auto to_condition = -Signed_Size(i - initial_size) - 1;
                out[i] = ins::Relative_Jump { { h }, to_condition };
            }
        }

        const auto back_offset = -Signed_Size(out.size() - initial_size + 1);
        out.push_back(ins::Relative_Jump { { h }, back_offset });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h, const ast::Jump& node)
    {
        if (node.token().type == Token_Type::keyword_break) {
            out.push_back(ins::Break { { h } });
            return {};
        }
        if (node.token().type == Token_Type::keyword_continue) {
            out.push_back(ins::Continue { { h } });
            return {};
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("jump nodes must only be break or continue");
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::Return_Statement& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            out.push_back(ins::Return { { h } });
            return {};
        }

        // Empty return statements produce Void, so the value is always known.
        // We should have early-returned already.
        BIT_MANIPULATION_ASSERT(node.get_expression());
        auto r = generate_code(node.get_expression());
        if (!r) {
            return r;
        }
        out.push_back(ins::Return { { h } });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h, const ast::Assignment& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        auto result = generate_code(node.get_expression());
        if (!result) {
            return result;
        }
        out.push_back(ins::Store { { h }, node.lookup_result });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*,
                                               const ast::Block_Statement& node)
    {
        const auto initial_size = out.size();
        for (ast::Some_Node* child : node.get_children()) {
            auto r = generate_code(child);
            if (!r) {
                BIT_MANIPULATION_ASSERT(initial_size <= out.size());
                out.resize(initial_size);
                return r;
            }
        }
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::If_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
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
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });

        const auto size_before_left = out.size();
        auto left = generate_code(node.get_left());
        if (!left) {
            restore();
            return left;
        }
        std::get<ins::Relative_Jump_If>(out[blank_jump_to_right_index]).offset
            = Signed_Size(out.size() - size_before_left + 1);

        const Size blank_jump_past_right_index = out.size();
        out.push_back(ins::Relative_Jump { { h }, 0 });
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

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::Binary_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
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

        if (node.get_op() == Token_Type::logical_and || node.get_op() == Token_Type::logical_or) {
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
            const bool circuit_breaker = node.get_op() == Token_Type::logical_or;
            const Size blank_jump_to_circuit_break_index = out.size();
            out.push_back(ins::Relative_Jump_If { { h }, 0, circuit_breaker });

            const auto size_before_right = out.size();
            auto right = generate_code(node.get_right());
            if (!right) {
                restore();
                return right;
            }
            std::get<ins::Relative_Jump_If>(out[blank_jump_to_circuit_break_index]).offset
                = Signed_Size(out.size() - size_before_right + 1);

            out.push_back(ins::Relative_Jump { { h }, 1 });
            out.push_back(
                ins::Push { { h }, Concrete_Value { Concrete_Type::Bool, circuit_breaker } });
        }
        else {
            auto right = generate_code(node.get_right());
            if (!right) {
                restore();
                return right;
            }
            out.push_back(ins::Binary_Operate { { h }, node.get_op() });
        }

        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::Prefix_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            return {};
        }

        auto init = generate_code(node.get_expression());
        if (!init) {
            return init;
        }
        out.push_back(ins::Unary_Operate { { h }, node.get_op() });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::Function_Call_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            if (!node.is_statement()) {
                out.push_back({ ins::Push { { h }, node.const_value()->concrete_value() } });
            }
            return {};
        }

        const Size restore_size = out.size();
        for (ast::Some_Node* arg : node.get_children()) {
            auto arg_code = generate_code(arg);
            if (!arg_code) {
                BIT_MANIPULATION_ASSERT(restore_size <= out.size());
                out.resize(restore_size);
                return arg_code;
            }
        }

        if (const auto* const called = std::get_if<ast::Function>(node.lookup_result)) {
            if (!called->const_value() || called->vm_address == ast::Function::invalid_vm_address) {
                return Analysis_Error { Analysis_Error_Code::codegen_call_to_unanalyzed, h,
                                        node.lookup_result };
            }
            out.push_back(ins::Call { { h }, called->vm_address });
        }
        if (const auto* const called = std::get_if<ast::Builtin_Function>(node.lookup_result)) {
            BIT_MANIPULATION_ASSERT(called->const_value().has_value());
            out.push_back(ins::Builtin_Call { { h }, called->get_function() });
        }

        if (node.is_statement()) {
            out.push_back(ins::Pop { { h } });
        }
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h,
                                               const ast::Id_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        auto instruction = node.const_value()->is_known()
            ? Instruction { ins::Push { { h }, node.const_value()->concrete_value() } }
            : Instruction { ins::Load { { h }, node.lookup_result } };
        out.push_back(instruction);
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node* h, const ast::Literal& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        BIT_MANIPULATION_ASSERT(node.const_value()->is_known());

        out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
        return {};
    }

    Result<void, Analysis_Error> generate_code(const ast::Some_Node*, const ast::Builtin_Function&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "codegen should not attempt to generate builtin function code");
        return {};
    }
};

} // namespace

Result<void, Analysis_Error> generate_code(std::pmr::vector<Instruction>& out,
                                           const ast::Function& function)
{
    Virtual_Code_Generator gen { out };
    return gen(function);
}

} // namespace bit_manipulation::bms