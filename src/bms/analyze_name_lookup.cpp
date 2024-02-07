#include <unordered_map>
#include <vector>

#include "bms/analyze.hpp"
#include "bms/parse.hpp"

namespace bit_manipulation::bms {

namespace {

struct Symbol_Table {
private:
    using map_type = std::unordered_map<std::string_view, ast::Handle>;
    map_type m_symbols;
    Symbol_Table* m_parent = nullptr;
    std::vector<Symbol_Table> m_children;

public:
    Symbol_Table() = default;

    Symbol_Table& push()
    {
        Symbol_Table& result = m_children.emplace_back();
        result.m_parent = this;
        return result;
    }

    std::variant<map_type::iterator, ast::Handle> emplace(std::string_view symbol, ast::Handle node)
    {
        BIT_MANIPULATION_ASSERT(node != ast::Handle::null);
        if (m_parent != nullptr) {
            if (std::optional<ast::Handle> old = m_parent->find(symbol)) {
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

    std::optional<ast::Handle> find(std::string_view symbol)
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

/// @brief Class responsible for performing name lookup.
/// This involves detecting lookup of undefined variables, duplicate variables, and other name
/// lookup mistakes.
/// After running this analyzer, every AST node that performs name lookup
/// (id-expressions and function calls) will have their `lookup_result` member point to the
/// looked up node.
struct Name_Lookup_Analyzer : Analyzer_Base {
private:
    Symbol_Table m_symbols;
    ast::Function* m_current_function = nullptr;

public:
    Name_Lookup_Analyzer(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        return analyze_symbols_global(m_program.root_node, m_root);
    }

private:
    template <typename T>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Handle, T&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Illegal AST node in global scope");
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Handle handle, ast::Program& n)
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Handle::null);
        for (ast::Handle decl : n.declarations) {
            auto r = std::visit(
                [this, decl]<typename T>(T& node) -> Result<void, Analysis_Error> {
                    return analyze_symbols_global(decl, node);
                },
                get_node(decl));
            if (!r) {
                return r;
            }
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Handle handle, ast::Const& n)
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Handle::null);
        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (auto* old = std::get_if<ast::Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_global_const, n.token,
                                    get_token(get_node(*old)) };
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Handle handle, ast::Function& n)
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Handle::null);

        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (auto* old = std::get_if<ast::Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_function, n.token,
                                    get_token(get_node(*old)) };
        }

        m_current_function = &n;
        return analyze_symbols_local(handle, m_symbols.push(), n);
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(ast::Handle handle, ast::Static_Assert& n)
    {
        return analyze_symbols_local(handle, m_symbols, n);
    }

    Result<void, Analysis_Error> analyze_symbols_local(ast::Handle handle, Symbol_Table& table)
    {
        if (handle == ast::Handle::null) {
            return {};
        }
        return std::visit([this, handle, &table](
                              auto& node) { return analyze_symbols_local(handle, table, node); },
                          get_node(handle));
    }

    Result<void, Analysis_Error> analyze_all_symbols_local(std::span<const ast::Handle> handles,
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
    Result<void, Analysis_Error> analyze_symbols_local(ast::Handle, Symbol_Table& table, T& node)
    {
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle handle, Symbol_Table& table, ast::Parameter& node)
    {
        auto it_or_handle = m_symbols.emplace(node.name, handle);
        if (auto* old = std::get_if<ast::Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_parameter, node.token,
                                    get_token(get_node(*old)) };
        }
        auto& type_node = std::get<ast::Type>(get_node(node.get_type()));
        ast::Handle g = type_node.get_width();
        if (g == ast::Handle::null) {
            return {};
        }
        if (auto r = analyze_symbols_local(g, table); !r) {
            const auto error = r.error();
            BIT_MANIPULATION_ASSERT(error.code
                                    == Analysis_Error_Code::reference_to_undefined_variable);
            if (auto* id = std::get_if<ast::Id_Expression>(&get_node(g))) {
                table.emplace(error.fail_token.extract(m_program.source), g);
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

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle, Symbol_Table& table, ast::Block_Statement& node)
    {
        return analyze_all_symbols_local(node.get_children(), table.push());
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle h, Symbol_Table& table, ast::Const& node)
    {
        auto it_or_handle = m_symbols.emplace(node.name, h);
        if (ast::Handle* old = std::get_if<ast::Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_variable, node.token,
                                    get_token(get_node(*old)) };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle h, Symbol_Table& table, ast::Let& node)
    {
        auto it_or_handle = m_symbols.emplace(node.name, h);
        if (ast::Handle* old = std::get_if<ast::Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_variable, node.token,
                                    get_token(get_node(*old)) };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle, Symbol_Table& table, ast::Static_Assert& node)
    {
        return analyze_symbols_local(node.get_expression(), table.push());
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle, Symbol_Table& table, ast::Assignment& node)
    {
        if (std::optional<ast::Handle> result = table.find(node.name)) {
            node.lookup_result = *result;
            return analyze_symbols_local(node.get_expression(), table);
        }
        return Analysis_Error { Analysis_Error_Code::assignment_of_undefined_variable, node.token };
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle, Symbol_Table& table, ast::Function_Call_Expression& node)
    {
        if (std::optional<ast::Handle> result = table.find(node.function)) {
            node.lookup_result = *result;
            return analyze_all_symbols_local(node.arguments, table);
        }
        return Analysis_Error { Analysis_Error_Code::call_to_undefined_function, node.token };
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(ast::Handle, Symbol_Table& table, ast::Id_Expression& node)
    {
        std::string_view name = node.token.extract(m_program.source);
        if (std::optional<ast::Handle> result = table.find(name)) {
            node.lookup_result = *result;
            return {};
        }
        return Analysis_Error { Analysis_Error_Code::reference_to_undefined_variable, node.token };
    }
};

} // namespace

Result<void, Analysis_Error> analyze_name_lookup(Parsed_Program& program)
{
    return Name_Lookup_Analyzer(program)();
}

} // namespace bit_manipulation::bms