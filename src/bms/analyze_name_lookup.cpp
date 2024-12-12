#include <unordered_map>
#include <vector>

#include "common/variant.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/evaluation/builtin_function.hpp"
#include "bms/evaluation/operations.hpp"
#include "bms/symbol_table.hpp"

namespace bit_manipulation::bms {

namespace {

const Symbol_Table builtin_symbols { { "assert", { Builtin_Function::assert } } };

constexpr bool shadowing = false;

/// @brief Class responsible for performing name lookup.
/// This involves detecting lookup of undefined variables, duplicate variables, and other name
/// lookup mistakes.
/// After running this analyzer, every AST node that performs name lookup
/// (id-expressions and function calls) will have their `lookup_result` member point to the
/// looked up node.
struct Name_Lookup_Analyzer {
private:
    Analyzed_Program& m_program;
    std::pmr::unsynchronized_pool_resource m_memory_resource;
    Symbol_Table m_symbols { Symbol_Table::From_Parent_Tag {}, builtin_symbols,
                             &m_memory_resource };
    ast::Function* m_current_function = nullptr;

public:
    Name_Lookup_Analyzer(Analyzed_Program& program, std::pmr::memory_resource* memory_resource)
        : m_program(program)
        , m_memory_resource(memory_resource)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        auto& program = get<ast::Program>(*m_program.get_root());

        auto first_result = run_pass<Register_Global_Declarations>(program);
        if (!first_result) {
            return first_result;
        }

        return run_pass<Analyze_Symbols_Global>(get<ast::Program>(*m_program.get_root()));
    }

private:
    template <typename F>
    Result<void, Analysis_Error> run_pass(ast::Program& program)
    {
        for (ast::Some_Node* decl : program.get_children()) {
            auto r = visit(F { *this, decl }, *decl);
            if (!r) {
                return r;
            }
        }
        return {};
    }

    struct Register_Global_Declarations {
        Name_Lookup_Analyzer& self;
        ast::Some_Node* handle;

        Result<void, Analysis_Error> operator()(ast::Const& n)
        {
            BIT_MANIPULATION_ASSERT(handle != nullptr);
            auto it_or_handle = self.m_symbols.emplace(n.get_name(), handle, shadowing);
            if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
                return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_global_const }
                    .fail(handle)
                    .cause(*old)
                    .build();
            }
            return {};
        }

        Result<void, Analysis_Error> operator()(ast::Function& n)
        {
            BIT_MANIPULATION_ASSERT(handle != nullptr);
            auto it_or_handle = self.m_symbols.emplace(n.get_name(), handle, shadowing);
            if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
                return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_function }
                    .fail(handle)
                    .cause(*old)
                    .build();
            }
            return {};
        }

        Result<void, Analysis_Error> operator()(ast::Static_Assert&)
        {
            BIT_MANIPULATION_ASSERT(handle != nullptr);
            return {};
        }

        Result<void, Analysis_Error> operator()(Ignore)
        {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Corrupted AST");
        }
    };

    struct Analyze_Symbols_Global {
        Name_Lookup_Analyzer& self;
        ast::Some_Node* handle;

        Result<void, Analysis_Error> operator()(ast::Const& n)
        {
            BIT_MANIPULATION_ASSERT(self.m_symbols.find(n.get_name()) == handle);

            return self.analyze_all_symbols_local(n.get_children(), self.m_symbols);
        }

        Result<void, Analysis_Error> operator()(ast::Function& n)
        {
            BIT_MANIPULATION_ASSERT(self.m_symbols.find(n.get_name()) == handle);

            self.m_current_function = &n;
            Symbol_Table local_symbols { Symbol_Table::From_Parent_Tag {}, self.m_symbols };
            return self.analyze_symbols_local(handle, local_symbols, n);
        }

        Result<void, Analysis_Error> operator()(ast::Static_Assert& n)
        {
            return self.analyze_symbols_local(handle, self.m_symbols, n);
        }

        Result<void, Analysis_Error> operator()(Ignore)
        {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Corrupted AST");
        }
    };

    Result<void, Analysis_Error> analyze_symbols_local(ast::Some_Node* handle, Symbol_Table& table)
    {
        if (handle == nullptr) {
            return {};
        }
        return visit(
            [this, handle, &table](auto& node) { //
                return analyze_symbols_local(handle, table, node);
            },
            *handle);
    }

    Result<void, Analysis_Error> analyze_all_symbols_local(std::span<ast::Some_Node* const> handles,
                                                           Symbol_Table& table)
    {
        for (auto h : handles) {
            if (auto r = analyze_symbols_local(h, table); !r) {
                return r;
            }
        }
        return {};
    }

    template <typename T>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node*, Symbol_Table& table, T& node)
    {
        return analyze_all_symbols_local(node.get_children(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node*, Symbol_Table& table, ast::Function& node)
    {
        for (Parameter& parameter : node.get_parameters()) {
            if (auto r = analyze_parameter(table, parameter); !r) {
                return r;
            }
        }
        if (auto* return_type = node.get_return_type_node()) {
            if (auto r = analyze_symbols_local(return_type, table, get<ast::Type>(*return_type));
                !r) {
                return r;
            }
        }
        if (auto r = analyze_symbols_local(node.get_requires_clause_node(), table); !r) {
            return r;
        }
        if (auto r = analyze_symbols_local(node.get_body_node(), table, node.get_body()); !r) {
            return r;
        }
        return {};
    }

    Result<void, Analysis_Error> analyze_parameter(Symbol_Table& table, Parameter& parameter)
    {
        auto it_or_handle = table.emplace(parameter.get_name(), &parameter, shadowing);
        if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
            return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_parameter }
                .fail(parameter)
                .cause(*old)
                .build();
        }
        ast::Some_Node* g = parameter.get_type().get_width_node();
        if (g == nullptr) {
            return {};
        }
        if (auto r = analyze_symbols_local(g, table); !r) {
            BIT_MANIPULATION_ASSERT(r.error().code()
                                    == Analysis_Error_Code::reference_to_undefined_variable);
            if (auto* id = get_if<ast::Id_Expression>(g)) {
                // TODO: now that lookup results can be more than just an AST node,
                //       we can get rid of this dirty hack and properly emplace a lookup result
                //       which represents a bit-generic parameter or something
                table.emplace(id->get_identifier(), g, shadowing);
                id->bit_generic = true;
                BIT_MANIPULATION_ASSERT(m_current_function != nullptr);
                m_current_function->is_generic = true;
            }
            else {
                return r;
            }
        }
        return {};
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node*, Symbol_Table& table, ast::Block_Statement& node)
    {
        Symbol_Table symbols_in_block { Symbol_Table::From_Parent_Tag {}, table };
        return analyze_all_symbols_local(node.get_children(), symbols_in_block);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Const& node)
    {
        auto it_or_handle = table.emplace(node.get_name(), h, shadowing);
        if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
            return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_variable }
                .fail(h)
                .cause(*old)
                .build();
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Let& node)
    {
        auto it_or_handle = table.emplace(node.get_name(), h, shadowing);
        if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
            return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_variable }
                .fail(h)
                .cause(*old)
                .build();
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node*, Symbol_Table& table, ast::Static_Assert& node)
    {
        return analyze_symbols_local(node.get_expression_node(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Assignment& node)
    {
        if (Optional_Lookup_Result result = table.find(node.get_name())) {
            node.lookup_result = *result;
            return analyze_symbols_local(node.get_expression_node(), table);
        }
        return Analysis_Error_Builder { Analysis_Error_Code::assignment_of_undefined_variable }
            .fail(h)
            .build();
    }

    Result<void, Analysis_Error> analyze_symbols_local(ast::Some_Node* h,
                                                       Symbol_Table& table,
                                                       ast::Function_Call_Expression& node)
    {
        if (Optional_Lookup_Result result = table.find(node.get_name())) {
            node.lookup_result = *result;
            return analyze_all_symbols_local(node.get_children(), table);
        }
        return Analysis_Error_Builder { Analysis_Error_Code::call_to_undefined_function }
            .fail(h)
            .build();
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Id_Expression& node)
    {
        if (Optional_Lookup_Result result = table.find(node.get_identifier())) {
            node.lookup_result = *result;
            return {};
        }
        return Analysis_Error_Builder { Analysis_Error_Code::reference_to_undefined_variable }
            .fail(h)
            .build();
    }
};

} // namespace

Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource)
{
    return Name_Lookup_Analyzer(program, memory_resource)();
}

} // namespace bit_manipulation::bms
