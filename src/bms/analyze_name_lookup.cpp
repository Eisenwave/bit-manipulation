#include <unordered_map>
#include <vector>

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"

namespace bit_manipulation::bms {

namespace {

struct Symbol_Table {
public:
    struct From_Parent_Tag { };
    using map_type = std::pmr::unordered_map<std::string_view, ast::Some_Node*>;

private:
    map_type m_symbols;
    const Symbol_Table* m_parent = nullptr;

public:
    Symbol_Table(From_Parent_Tag, const Symbol_Table& parent, std::pmr::memory_resource* memory)
        : m_symbols(memory)
        , m_parent(&parent)
    {
    }

    Symbol_Table(From_Parent_Tag, const Symbol_Table& parent)
        : Symbol_Table(From_Parent_Tag {}, parent, parent.m_symbols.get_allocator().resource())
    {
    }

    explicit Symbol_Table(std::pmr::memory_resource* memory)
        : m_symbols(memory)
    {
    }

    explicit Symbol_Table(std::initializer_list<map_type::value_type> init,
                          std::pmr::memory_resource* memory = std::pmr::get_default_resource())
        : m_symbols(init, memory)
    {
    }

    Symbol_Table(const Symbol_Table&) = delete;
    Symbol_Table(Symbol_Table&&) = default;

    Symbol_Table& operator=(const Symbol_Table&) = delete;
    Symbol_Table& operator=(Symbol_Table&&) = default;

    std::variant<map_type::iterator, ast::Some_Node*> emplace(std::string_view symbol,
                                                              ast::Some_Node* node)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        if (m_parent != nullptr) {
            if (std::optional<ast::Some_Node*> old = m_parent->find(symbol)) {
                return *old;
            }
        }
        auto [iter, success] = m_symbols.emplace(symbol, node);
        if (success) {
            return iter;
        }
        else {
            return iter->second;
        }
    }

    std::optional<ast::Some_Node*> find(std::string_view symbol) const
    {
        if (m_parent != nullptr) {
            if (auto parent_result = m_parent->find(symbol)) {
                return parent_result;
            }
        }
        if (auto iter = m_symbols.find(symbol); iter != m_symbols.end()) {
            return iter->second;
        }
        return std::nullopt;
    }
};

template <Builtin_Function F>
ast::Some_Node builtin_function_node = ast::Builtin_Function(F);

const Symbol_Table builtin_symbols { { "assert",
                                       &builtin_function_node<Builtin_Function::assert> } };

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
        return analyze_symbols_global(m_program.get_root(),
                                      std::get<ast::Program>(*m_program.get_root()));
    }

private:
    template <typename T>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Some_Node*, T&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Illegal AST node in global scope");
    }

    Result<void, Analysis_Error> analyze_symbols_global(ast::Some_Node* handle, ast::Program& n)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);
        for (ast::Some_Node* decl : n.get_children()) {
            auto r = fast_visit(
                [this, decl]<typename T>(T& node) -> Result<void, Analysis_Error> {
                    return analyze_symbols_global(decl, node);
                },
                *decl);
            if (!r) {
                return r;
            }
        }
        return {};
    }

    Result<void, Analysis_Error> analyze_symbols_global(ast::Some_Node* handle, ast::Const& n)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);
        auto it_or_handle = m_symbols.emplace(n.get_name(), handle);
        if (auto* old = std::get_if<ast::Some_Node*>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_global_const, handle,
                                    *old };
        }
        return {};
    }

    Result<void, Analysis_Error> analyze_symbols_global(ast::Some_Node* handle, ast::Function& n)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);

        auto it_or_handle = m_symbols.emplace(n.get_name(), handle);
        if (auto* old = std::get_if<ast::Some_Node*>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_function, handle, *old };
        }

        m_current_function = &n;
        Symbol_Table local_symbols { Symbol_Table::From_Parent_Tag {}, m_symbols };
        return analyze_symbols_local(handle, local_symbols, n);
    }

    Result<void, Analysis_Error> analyze_symbols_global(ast::Some_Node* handle,
                                                        ast::Static_Assert& n)
    {
        return analyze_symbols_local(handle, m_symbols, n);
    }

    Result<void, Analysis_Error> analyze_symbols_local(ast::Some_Node* handle, Symbol_Table& table)
    {
        if (handle == nullptr) {
            return {};
        }
        return fast_visit([this, handle, &table](
                              auto& node) { return analyze_symbols_local(handle, table, node); },
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
    analyze_symbols_local(ast::Some_Node* handle, Symbol_Table& table, ast::Parameter& node)
    {
        auto it_or_handle = table.emplace(node.get_name(), handle);
        if (auto* old = std::get_if<ast::Some_Node*>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_parameter, handle, *old };
        }
        auto& type_node = std::get<ast::Type>(*node.get_type());
        ast::Some_Node* g = type_node.get_width();
        if (g == nullptr) {
            return {};
        }
        if (auto r = analyze_symbols_local(g, table); !r) {
            const auto error = r.error();
            BIT_MANIPULATION_ASSERT(error.code()
                                    == Analysis_Error_Code::reference_to_undefined_variable);
            if (auto* id = std::get_if<ast::Id_Expression>(g)) {
                table.emplace(id->get_identifier(), g);
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
        auto it_or_handle = table.emplace(node.get_name(), h);
        if (auto* old = std::get_if<ast::Some_Node*>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_variable, h, *old };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Let& node)
    {
        auto it_or_handle = table.emplace(node.get_name(), h);
        if (auto* old = std::get_if<ast::Some_Node*>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_variable, h, *old };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node*, Symbol_Table& table, ast::Static_Assert& node)
    {
        return analyze_symbols_local(node.get_expression(), table);
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Assignment& node)
    {
        if (std::optional<ast::Some_Node*> result = table.find(node.name)) {
            node.lookup_result = *result;
            return analyze_symbols_local(node.get_expression(), table);
        }
        return Analysis_Error { Analysis_Error_Code::assignment_of_undefined_variable, h };
    }

    Result<void, Analysis_Error> analyze_symbols_local(ast::Some_Node* h,
                                                       Symbol_Table& table,
                                                       ast::Function_Call_Expression& node)
    {
        if (std::optional<ast::Some_Node*> result = table.find(node.get_name())) {
            node.lookup_result = *result;
            return analyze_all_symbols_local(node.get_children(), table);
        }
        return Analysis_Error { Analysis_Error_Code::call_to_undefined_function, h };
    }

    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Some_Node* h, Symbol_Table& table, ast::Id_Expression& node)
    {
        if (std::optional<ast::Some_Node*> result = table.find(node.get_identifier())) {
            node.lookup_result = *result;
            return {};
        }
        return Analysis_Error { Analysis_Error_Code::reference_to_undefined_variable, h };
    }
};

} // namespace

Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource)
{
    return Name_Lookup_Analyzer(program, memory_resource)();
}

} // namespace bit_manipulation::bms