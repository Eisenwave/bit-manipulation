#include "bms/analyze.hpp"
#include "bms/vm.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

struct Virtual_Code_Generator : Analyzer_Base {
    using Result_Type = Result<std::vector<Instruction>, Analysis_Error>;

    Virtual_Code_Generator(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

public:
    Result_Type operator()(Node_Handle h, Function_Node& function)
    {
        return generate_code(h, function);
    }

private:
    Result_Type generate_code(Node_Handle h)
    {
        return std::visit([this, h](auto& node) { return generate_code(h, node); }, get_node(h));
    }

    template <typename T>
    Result_Type generate_code(Node_Handle h, T& node)
    {
        std::vector<Instruction> result;
        for (Node_Handle child : node.get_children()) {
            auto r = generate_code(child);
            if (!r) {
                return r;
            }
            result.insert(result.end(), r->begin(), r->end());
        }
        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, Program_Node&)
    {
        BIT_MANIPULATION_ASSERT(false);
    }

    template <>
    Result_Type generate_code(Node_Handle h, Let_Const_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            return std::vector<Instruction> { ins::Push { node.const_value->concrete_value() },
                                              ins::Store { h } };
        }

        auto init = generate_code(node.get_initializer());
        if (!init) {
            return init;
        }
        init->push_back(ins::Store { h });
        return init;
    }

    template <>
    Result_Type generate_code(Node_Handle h, If_Statement_Node& node)
    {
        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }
        auto if_code = generate_code(node.get_if_block());
        if (!if_code) {
            return if_code;
        }

        std::vector<Instruction> result = std::move(*condition);
        result.push_back(ins::Jump_If { Signed_Size(if_code->size()), false });
        result.insert(result.end(), if_code->begin(), if_code->end());

        if (node.get_else_block() != Node_Handle::null) {
            auto else_code = generate_code(node.get_else_block());
            if (!else_code) {
                return else_code;
            }
            result.push_back(ins::Jump { Signed_Size(else_code->size()) });
            result.insert(result.end(), else_code->begin(), else_code->end());
        }

        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, While_Statement_Node& node)
    {
        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }
        auto block = generate_code(node.get_block());
        if (!block) {
            return block;
        }

        // TODO: convert continue and break into non-symbolic jumps

        std::vector<Instruction> result = std::move(*condition);
        result.push_back(ins::Jump_If { Signed_Size(block->size()), false });
        result.insert(result.end(), block->begin(), block->end());

        const auto back_offset = -Signed_Size(block->size() + condition->size() + 2);
        result.push_back(ins::Jump { back_offset });
        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, Jump_Node& node)
    {
        if (node.token.type == Token_Type::keyword_break) {
            return std::vector<Instruction> { ins::Break {} };
        }
        if (node.token.type == Token_Type::keyword_continue) {
            return std::vector<Instruction> { ins::Break {} };
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    template <>
    Result_Type generate_code(Node_Handle h, Assignment_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            return std::vector<Instruction> { ins::Push { node.const_value->concrete_value() },
                                              ins::Store { node.lookup_result } };
        }
        auto result = generate_code(node.get_expression());
        if (!result) {
            return result;
        }
        result->push_back(ins::Store { node.lookup_result });
        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, If_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            return std::vector<Instruction> { ins::Push { node.const_value->concrete_value() } };
        }
        auto condition = generate_code(node.get_condition());
        if (!condition) {
            return condition;
        }
        auto left = generate_code(node.get_left());
        if (!left) {
            return left;
        }
        auto right = generate_code(node.get_right());
        if (!right) {
            return right;
        }

        std::vector<Instruction> result = std::move(*condition);
        result.push_back(ins::Jump_If { Signed_Size(left->size()), false });
        result.insert(result.end(), left->begin(), left->end());
        result.push_back(ins::Jump { Signed_Size(right->size()) });
        result.insert(result.end(), right->begin(), right->end());

        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, Binary_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            return std::vector<Instruction> { ins::Push { node.const_value->concrete_value() } };
        }

        auto left = generate_code(node.get_left());
        if (!left) {
            return left;
        }
        auto right = generate_code(node.get_right());
        if (!right) {
            return right;
        }
        std::vector<Instruction> result;
        result.insert(result.end(), left->begin(), left->end());
        result.insert(result.end(), right->begin(), right->end());
        result.push_back(ins::Binary_Operate { node.op });
        return result;
    }

    template <>
    Result_Type generate_code(Node_Handle h, Prefix_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        if (node.const_value->int_value) {
            return std::vector<Instruction> { ins::Push { node.const_value->concrete_value() } };
        }

        auto init = generate_code(node.get_expression());
        if (!init) {
            return init;
        }
        init->push_back(ins::Unary_Operate { node.op });
        return init;
    }

    template <>
    Result_Type generate_code(Node_Handle h, Id_Expression_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        auto instruction = node.const_value->int_value
            ? Instruction { ins::Push { node.const_value->concrete_value() } }
            : Instruction { ins::Load { node.lookup_result } };
        return std::vector<Instruction> { instruction };
    }

    template <>
    Result_Type generate_code(Node_Handle h, Literal_Node& node)
    {
        BIT_MANIPULATION_ASSERT(node.const_value);
        BIT_MANIPULATION_ASSERT(node.const_value->int_value);

        Instruction instruction = ins::Push { node.const_value->concrete_value() };
        return std::vector<Instruction> { instruction };
    }
};

} // namespace bit_manipulation::bms