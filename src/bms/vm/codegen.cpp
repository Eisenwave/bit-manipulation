#include <ranges>

#include "common/variant.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/evaluation/builtin_function.hpp"
#include "bms/vm/codegen.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/vm.hpp"

namespace bit_manipulation::bms {

namespace {

[[nodiscard]] Concrete_Type get_parameter_type(Lookup_Result some_function, Size i)
{
    if (const auto* node = get_if<ast::Some_Node*>(&some_function)) {
        if (const auto* f = get_if<ast::Function>(*node)) {
            BIT_MANIPULATION_ASSERT(i < f->get_parameter_count());
            const std::optional<Value>& value = f->get_parameters()[i].get_type().const_value();
            BIT_MANIPULATION_ASSERT(value);
            return value->get_type();
        }
    }

    if (const auto* f = get_if<Builtin_Function>(&some_function)) {
        const std::span<const Concrete_Type> parameters = builtin_parameter_types(*f);
        BIT_MANIPULATION_ASSERT(i < parameters.size());
        return parameters[i];
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE();
}

[[nodiscard]] const void* to_target(const Lookup_Result& result)
{
    if (const auto* const* node = get_if<ast::Some_Node*>(&result)) {
        return *node;
    }
    if (const auto* const* node = get_if<Parameter*>(&result)) {
        return *node;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Loads must happen from AST nodes or parameters.");
}

struct Virtual_Code_Generator {
private:
    std::pmr::vector<Instruction>& out;
    Call_Policy m_call_policy;
    std::optional<Concrete_Type> m_return_type;

public:
    [[nodiscard]] explicit Virtual_Code_Generator(std::pmr::vector<Instruction>& out,
                                                  Call_Policy call_policy)
        : out(out)
        , m_call_policy(call_policy)
    {
    }

public:
    void operator()(const ast::Some_Node* h, const ast::Function& function)
    {
        return generate_code(h, function);
    }

private:
    void generate_code(const ast::Some_Node* h)
    {
        BIT_MANIPULATION_ASSERT(h);
        return visit([this, h](const auto& node) { return generate_code(h, node); }, *h);
    }

    void generate_code(const ast::Some_Node*, const ast::Program&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("codegen starts at the function level");
    }

    void generate_code(const ast::Some_Node* h, const ast::Function& function)
    {
        std::span<const Parameter> parameters = function.get_parameters();
        for (Size i = parameters.size(); i-- != 0;) {
            // We use left-to-right push order, so storing the parameters in variables upon
            // function entry happens in reverse order.
            out.push_back(ins::Store { { parameters[i].get_debug_info() }, &parameters[i] });
        }

        m_return_type = function.get_concrete_return_type();
        // TODO: add Scope_Exit to clean this up upon return (just for robustness, not critical)

        generate_code(function.get_body_node(), function.get_body());

        if (m_return_type == Concrete_Type::Void) {
            BIT_MANIPULATION_ASSERT(function.definitely_returns != Tribool::maybe);
            if (function.definitely_returns == Tribool::fawse) {
                out.push_back(ins::Push { { h }, Concrete_Value::Void });
                out.push_back(ins::Return { { h } });
            }
        }
    }

    void generate_code(const ast::Some_Node*, const ast::Type&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "codegen cannot reach type nodes because their parents handle it");
    }

    void generate_code(const ast::Some_Node*, const ast::Const& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        // Const nodes don't produce any codegen because any id expressions that access constants
        // will be constant-folded and emit a `Push` instead.
    }

    void generate_code(const ast::Some_Node* h, const ast::Let& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (!node.get_initializer_node()) {
            return;
        }
        generate_code(node.get_initializer_node());
        out.push_back(ins::Store { { h }, h });
    }

    void generate_code(const ast::Some_Node*, const ast::Static_Assert& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
    }

    void generate_code(const ast::Some_Node* h, const ast::If_Statement& node)
    {
        generate_code(node.get_condition_node());

        const Size blank_jump_to_else_index = out.size();
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });
        const auto size_before_if = out.size();
        generate_code(node.get_if_block_node());

        get<ins::Relative_Jump_If>(out[blank_jump_to_else_index]).offset
            = Signed_Size(out.size() - size_before_if + (node.get_else_node() != nullptr));

        if (node.get_else_node() == nullptr) {
            return;
        }

        const Size blank_jump_past_else_index = out.size();
        out.push_back(ins::Relative_Jump { { h }, 0 });
        const auto size_before_else = out.size();
        generate_code(node.get_else_node());

        get<ins::Relative_Jump>(out[blank_jump_past_else_index]).offset
            = Signed_Size(out.size() - size_before_else);
    }

    void generate_code(const ast::Some_Node* h, const ast::While_Statement& node)
    {
        const Size initial_size = out.size();

        generate_code(node.get_condition_node());

        const Size blank_jump_past_loop_index = out.size();
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });
        const auto size_before_block = out.size();

        generate_code(node.get_block_node());

        get<ins::Relative_Jump_If>(out[blank_jump_past_loop_index]).offset
            = Signed_Size(out.size() - size_before_block + 1);

        for (Size i = size_before_block; i < out.size(); ++i) {
            if (holds_alternative<ins::Break>(out[i])) {
                const auto past_the_loop = Signed_Size(out.size() - i);
                out[i] = ins::Relative_Jump { { h }, past_the_loop };
            }
            if (holds_alternative<ins::Continue>(out[i])) {
                const auto to_condition = -Signed_Size(i - initial_size) - 1;
                out[i] = ins::Relative_Jump { { h }, to_condition };
            }
        }

        const auto back_offset = -Signed_Size(out.size() - initial_size + 1);
        out.push_back(ins::Relative_Jump { { h }, back_offset });
    }

    void generate_code(const ast::Some_Node* h, const ast::Control_Statement& node)
    {
        BIT_MANIPULATION_ASSERT(m_return_type);
        BIT_MANIPULATION_ASSERT(node.const_value());

        if (node.is_break()) {
            out.push_back(ins::Break { { h } });
            return;
        }
        if (node.is_continue()) {
            out.push_back(ins::Continue { { h } });
            return;
        }

        BIT_MANIPULATION_ASSERT(node.is_return());
        BIT_MANIPULATION_ASSERT(node.const_value()->get_type() == *m_return_type);

        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            out.push_back(ins::Return { { h } });
            return;
        }

        // Empty return statements produce Void, so the value is always known.
        // We should have early-returned already.
        BIT_MANIPULATION_ASSERT(node.get_expression_node());
        generate_code(node.get_expression_node());

        const Concrete_Type expression_type
            = get_const_value(*node.get_expression_node()).value().get_type();
        if (expression_type != m_return_type) {
            out.push_back(ins::Convert { { h }, *m_return_type });
        }

        out.push_back(ins::Return { { h } });
    }

    void generate_code(const ast::Some_Node* h, const ast::Assignment& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        generate_code(node.get_expression_node());
        out.push_back(ins::Store { { h }, to_target(node.lookup_result.value()) });
    }

    void generate_code(const ast::Some_Node*, const ast::Block_Statement& node)
    {
        for (const ast::Some_Node* child : node.get_children()) {
            generate_code(child);
        }
    }

    void generate_code(const ast::Some_Node* h, const ast::Conversion_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            return;
        }

        generate_code(node.get_expression_node());

        const ast::Type& target_type = node.get_target_type();
        out.push_back(ins::Convert { { h }, target_type.concrete_type().value() });
    }

    void generate_code(const ast::Some_Node* h, const ast::If_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            return;
        }

        generate_code(node.get_condition_node());

        const Size blank_jump_to_right_index = out.size();
        out.push_back(ins::Relative_Jump_If { { h }, 0, false });

        const auto size_before_left = out.size();
        generate_code(node.get_left_node());

        get<ins::Relative_Jump_If>(out[blank_jump_to_right_index]).offset
            = Signed_Size(out.size() - size_before_left + 1);

        const Size blank_jump_past_right_index = out.size();
        out.push_back(ins::Relative_Jump { { h }, 0 });
        const auto size_before_right = out.size();
        generate_code(node.get_right_node());

        get<ins::Relative_Jump>(out[blank_jump_past_right_index]).offset
            = Signed_Size(out.size() - size_before_right);
    }

    void generate_code(const ast::Some_Node* h, const ast::Binary_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
        }

        generate_code(node.get_left_node());

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
            generate_code(node.get_right_node());

            get<ins::Relative_Jump_If>(out[blank_jump_to_circuit_break_index]).offset
                = Signed_Size(out.size() - size_before_right + 1);

            out.push_back(ins::Relative_Jump { { h }, 1 });
            out.push_back(
                ins::Push { { h }, Concrete_Value { Concrete_Type::Bool, circuit_breaker } });
        }
        else {
            generate_code(node.get_right_node());
            out.push_back(ins::Binary_Operate { { h }, node.get_expression_type() });
        }
    }

    void generate_code(const ast::Some_Node* h, const ast::Prefix_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
            return;
        }

        generate_code(node.get_expression_node());
        out.push_back(ins::Unary_Operate { { h }, node.get_expression_type() });
    }

    void generate_code(const ast::Some_Node* h, const ast::Function_Call_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        if (node.const_value()->is_known()) {
            if (!node.is_statement()) {
                out.push_back({ ins::Push { { h }, node.const_value()->concrete_value() } });
            }
        }

        const std::span<const ast::Some_Node* const> arguments = node.get_children();

        for (Size i = 0; i < arguments.size(); ++i) {
            generate_code(arguments[i]);
            const Concrete_Type argument_type = get_const_value(*arguments[i]).value().get_type();
            const Concrete_Type parameter_type = get_parameter_type(node.lookup_result.value(), i);
            if (argument_type != parameter_type) {
                out.push_back(ins::Convert { { h }, parameter_type });
            }
        }

        if (const auto* const* called_node = get_if<ast::Some_Node*>(&node.lookup_result)) {
            if (const auto* const called = get_if<ast::Function>(*called_node)) {
                BIT_MANIPULATION_ASSERT(called->was_analyzed());
                out.push_back(generate_call_instruction(h, *called));
            }
        }
        else if (const auto* const builtin = get_if<Builtin_Function>(&node.lookup_result)) {
            out.push_back(ins::Builtin_Call { { h }, *builtin });
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Impossible call.");
        }

        if (node.is_statement()) {
            out.push_back(ins::Pop { { h } });
        }
    }

    [[nodiscard]] Instruction generate_call_instruction(const ast::Some_Node* call_expression,
                                                        const ast::Function& called) const
    {
        BIT_MANIPULATION_ASSERT(called.was_analyzed());

        const ins::Symbolic_Call symbolic { { call_expression }, &called };
        if (m_call_policy == Call_Policy::always_symbolic) {
            return symbolic;
        }

        const std::optional<Size> address = called.get_vm_address();
        if (!address) {
            if (m_call_policy == Call_Policy::assert_resolve_possible
                || m_call_policy == Call_Policy::resolve) {
                BIT_MANIPULATION_ASSERT_UNREACHABLE("VM address is required by policy.");
            }
            return symbolic;
        }

        return ins::Call { { call_expression }, *address };
    }

    void generate_code(const ast::Some_Node* h, const ast::Id_Expression& node)
    {
        BIT_MANIPULATION_ASSERT(node.was_analyzed());
        auto instruction = node.const_value()->is_known()
            ? Instruction { ins::Push { { h }, node.const_value()->concrete_value() } }
            : Instruction { ins::Load { { h }, to_target(node.lookup_result.value()) } };
        out.push_back(instruction);
    }

    void generate_code(const ast::Some_Node* h, const ast::Literal& node)
    {
        BIT_MANIPULATION_ASSERT(node.was_analyzed());
        BIT_MANIPULATION_ASSERT(node.const_value()->is_known());

        out.push_back(ins::Push { { h }, node.const_value()->concrete_value() });
    }
};

} // namespace

void generate_code(std::pmr::vector<Instruction>& out,
                   const ast::Some_Node* function_node,
                   const ast::Function& function,
                   Call_Policy policy)
{
    BIT_MANIPULATION_ASSERT(&get<ast::Function>(*function_node) == &function);
    Virtual_Code_Generator gen { out, policy };
    gen(function_node, function);
}

void generate_code(std::pmr::vector<Instruction>& out,
                   const ast::Some_Node* function_node,
                   Call_Policy policy)
{
    generate_code(out, function_node, get<ast::Function>(*function_node), policy);
}

void generate_code(std::pmr::vector<Instruction>& out,
                   ast::Program& program,
                   Codegen_Options options)
{
    for (ast::Some_Node* decl_node : program.get_children()) {
        if (auto* function = get_if<ast::Function>(decl_node)) {
            BIT_MANIPULATION_ASSERT(function->was_analyzed());
            if (options.ignore_with_address && function->get_vm_address()) {
                continue;
            }
            const Size vm_address = out.size();
            Virtual_Code_Generator { out, options.calls }(decl_node, *function);
            // Generating code for a function should always result in at least
            // one instruction being emitted (namely a Return at the very least).
            BIT_MANIPULATION_ASSERT(out.size() > vm_address);
            if (options.write_vm_address) {
                function->set_vm_address(vm_address);
            }
        }
    }
}

void generate_code(Analyzed_Program& program, Codegen_Options options)
{
    auto& program_node = get<ast::Program>(*program.get_root());
    generate_code(program.get_vm().instructions(), program_node, options);
}

} // namespace bit_manipulation::bms
