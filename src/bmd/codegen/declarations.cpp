#include "common/variant.hpp"

#include "bmd/codegen/declarations.hpp"

namespace bit_manipulation::bmd {
namespace {

[[nodiscard]] Dependency_Type dependency_type_constant_recursive(bool constant, bool recursive)
{
    return constant
        ? (recursive ? Dependency_Type::constant_recursive : Dependency_Type::constant_direct)
        : (recursive ? Dependency_Type::normal_recursive : Dependency_Type::normal_direct);
}

struct Dependency_Gatherer {
    Function_Ref<bool(Dependency)>& out;
    bool constant : 1 = false;
    bool recursive : 1 = false;

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
            visit(in_constant_expression(true), *requires_clause);
        }
        in_constant_expression(false)(f.get_body());
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
            visit(in_constant_expression(false), *child);
        }
    }

    template <one_of<bms::ast::Type, bms::ast::Const, bms::ast::Static_Assert> T>
    void operator()(const T& block)
    {
        for (const bms::ast::Some_Node* child : block.get_children()) {
            if (child != nullptr) {
                visit(in_constant_expression(true), *child);
            }
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
    /// @brief Returns a copy of this `Dependency_Gatherer`, except that `constant` is set to
    /// `constant_expression` in the new object.
    /// This function should be used whenever entering or leaving a context that is a constant
    /// expression.
    [[nodiscard]] Dependency_Gatherer in_constant_expression(bool constant_expression)
    {
        return { .out = out, .constant = constant_expression, .recursive = recursive };
    }

    void emit(const bms::Optional_Lookup_Result& lookup_result)
    {
        if (const auto* const* looked_up_node = get_if<bms::ast::Some_Node*>(&lookup_result)) {
            emit(**looked_up_node);
            return;
        }
        if (const auto* const* looked_up_parameter = get_if<bms::Parameter*>(&lookup_result)) {
            (*this)((*looked_up_parameter)->get_type());
            return;
        }
        BIT_MANIPULATION_ASSERT(holds_alternative<bms::Builtin_Function>(lookup_result));
    }

    void emit(const bms::ast::Some_Node& some_node)
    {
        const Dependency_Type type = dependency_type_constant_recursive(constant, recursive);
        const bool should_recurse = out({ &some_node, type });
        if (should_recurse) {
            visit(Dependency_Gatherer { .out = out, .constant = constant, .recursive = true },
                  some_node);
        }
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
