#include "common/assert.hpp"
#include "common/variant.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"

namespace bit_manipulation::bms {

namespace {

struct Return_Analyzer {
    Analyzed_Program& m_program;
    const ast::Some_Node* m_node;

    Result<bool, Analysis_Error> analyze(const ast::Some_Node* node) const
    {
        return visit(Return_Analyzer { m_program, node }, *node);
    }

    Result<bool, Analysis_Error> operator()(const ast::Program& program) const
    {
        BIT_MANIPULATION_ASSERT(program.was_analyzed());
        for (const ast::Some_Node* child : program.get_children()) {
            auto r = analyze(child);
            if (!r) {
                return r;
            }
        }
        return true;
    }

    Result<bool, Analysis_Error> operator()(const ast::Function& function) const
    {
        if (function.is_generic) {
            for (const ast::Function::Instance& instance : function.instances) {
                if (auto r = Return_Analyzer { m_program, instance.handle }(instance.function());
                    !r) {
                    return r;
                }
            }
            return false;
        }
        // we intentionally don't require functions to have been analyzed here
        // because it's only required that the function body and return type was analyzed,
        // not the function itself
        auto r = Return_Analyzer { m_program, function.get_body_node() }(function.get_body());
        if (!r) {
            return r;
        }
        if (!*r && function.get_return_type().get_type() != Type_Type::Void) {
            return Analysis_Error_Builder { Analysis_Error_Code::no_return }.fail(m_node).build();
        }
        return r;
    }

    template <one_of<ast::Const,
                     ast::Let,
                     ast::Static_Assert,
                     ast::Assignment,
                     ast::Function_Call_Expression> T>
    Result<bool, Analysis_Error> operator()(const T& node) const
    {
        BIT_MANIPULATION_ASSERT(node.was_analyzed());
        return false;
    }

    Result<bool, Analysis_Error> operator()(const ast::Control_Statement& control) const
    {
        BIT_MANIPULATION_ASSERT(control.was_analyzed());
        return control.is_return();
    }

    Result<bool, Analysis_Error> operator()(const ast::Block_Statement& block) const
    {
        BIT_MANIPULATION_ASSERT(block.was_analyzed());
        std::span<const ast::Some_Node* const> children = block.get_children();
        for (Size i = children.size(); i-- != 0;) {
            auto r = analyze(children[i]);
            // this is effectively short-circuiting:
            // if any of the statements (starting from the end) of a block definitely returns,
            // we can say that the whole block returns
            if (!r) {
                return r;
            }
            if (!*r) {
                continue;
            }
            if (i + 1 != children.size()) {
                return Analysis_Error_Builder { Analysis_Error_Code::unreachable_code }
                    .fail(children[i + 1])
                    .cause(children[i])
                    .build();
            }
            return r;
        }
        return false;
    }

    Result<bool, Analysis_Error> operator()(const ast::If_Statement& statement) const
    {
        BIT_MANIPULATION_ASSERT(statement.was_analyzed());
        // The basic idea here is that an if statement definitely returns if
        // - it has an else statement, and
        // - both branches definitely return.
        if (!statement.get_else_node()) {
            return false;
        }
        auto if_result = Return_Analyzer { m_program, statement.get_if_block_node() }(
            statement.get_if_block());
        if (!if_result || !*if_result) {
            return if_result;
        }
        return analyze(statement.get_else_node());
    }

    Result<bool, Analysis_Error> operator()(const ast::While_Statement& loop) const
    {
        BIT_MANIPULATION_ASSERT(loop.was_analyzed());
        // Technically, this is incorrect.
        // Specifically, if a while statement has a constant condition (e.g. while true),
        // and if the body definitely returns, it also definitely returns.
        // However, we don't care about this case since it's kinda pointless to write anyway.
        return false;
    }

    Result<bool, Analysis_Error> operator()(Ignore) const
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Analysis should not have reached expressions etc.");
    }
};

} // namespace

Result<void, Analysis_Error> analyze_returning(Analyzed_Program& program)
{
    const auto& program_node = get<ast::Program>(*program.get_root());
    auto r = Return_Analyzer { program, program.get_root() }(program_node);
    if (!r) {
        return r.error();
    }
    return {};
}

Result<bool, Analysis_Error> analyze_returning(Analyzed_Program& program,
                                               const ast::Some_Node* function_node,
                                               const ast::Function& f)
{
    BIT_MANIPULATION_ASSERT(&get<ast::Function>(*function_node) == &f);
    // We cannot obtain a meaningful result for generic functions because whether they return
    // may depend on the individual instantiation.
    BIT_MANIPULATION_ASSERT(!f.is_generic);
    return Return_Analyzer { program, function_node }(f);
}

} // namespace bit_manipulation::bms
