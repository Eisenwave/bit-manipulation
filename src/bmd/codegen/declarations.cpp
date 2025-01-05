#include "common/variant.hpp"

#include "bmd/codegen/declarations.hpp"

namespace bit_manipulation::bmd {
namespace {

struct Dependency_Gatherer {
    Function_Ref<bool(Dependency)>& out;
    Dependency_Type default_type = Dependency_Type::normal;

    void operator()(const bms::ast::Program&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected AST structure.");
    }

    void operator()(const bms::ast::Function& f)
    {
        for (const auto& p : f.get_parameters()) {
            (*this)(p.get_type());
        }
        if (f.get_return_type_node()) {
            (*this)(f.get_return_type());
        }
        if (const auto* requires_clause = f.get_requires_clause_node()) {
            visit(Dependency_Gatherer { out, Dependency_Type::constant }, *requires_clause);
        }
        (*this)(f.get_body());
    }

    template <one_of<bms::ast::Let,
                     bms::ast::If_Statement,
                     bms::ast::While_Statement,
                     bms::ast::Control_Statement,
                     bms::ast::Assignment,
                     bms::ast::Block_Statement,
                     bms::ast::Conversion_Expression,
                     bms::ast::If_Expression,
                     bms::ast::Binary_Expression,
                     bms::ast::Prefix_Expression> T>
    void operator()(const T& block)
    {
        for (const bms::ast::Some_Node* child : block.get_children()) {
            visit(*this, *child);
        }
    }

    template <one_of<bms::ast::Type, bms::ast::Const, bms::ast::Static_Assert> T>
    void operator()(const T& block)
    {
        for (const bms::ast::Some_Node* child : block.get_children()) {
            visit(Dependency_Gatherer { out, Dependency_Type::constant }, *child);
        }
    }

    template <one_of<bms::ast::Function_Call_Expression, bms::ast::Id_Expression> T>
    void operator()(const T& node)
    {
        emit(node.lookup_result);
    }

    void operator()(const bms::ast::Literal&)
    {
        // literals have no dependencies
    }

private:
    void emit(const bms::Optional_Lookup_Result& lookup_result)
    {
        if (const auto* const* looked_up_node = get_if<bms::ast::Some_Node*>(&lookup_result)) {
            emit(*looked_up_node);
        }
        if (const auto* const* looked_up_parameter = get_if<bms::Parameter*>(&lookup_result)) {
            (*this)((*looked_up_parameter)->get_type());
        }
        BIT_MANIPULATION_ASSERT(holds_alternative<bms::Builtin_Function>(lookup_result));
    }

    void emit(const bms::ast::Some_Node* some_node)
    {
        out({ some_node, default_type });
    }
};

} // namespace

void for_each_global_dependency(Function_Ref<bool(Dependency)> out, const bms::ast::Some_Node& node)
{
    visit(Dependency_Gatherer { out }, node);
}

void for_each_direct_global_dependency(Function_Ref<void(Dependency)> out,
                                       const bms::ast::Some_Node& node)
{
    for_each_global_dependency(
        [&](Dependency d) -> bool {
            out(d);
            return false;
        },
        node);
}

} // namespace bit_manipulation::bmd
