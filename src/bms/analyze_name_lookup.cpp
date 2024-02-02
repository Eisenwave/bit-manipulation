#include <unordered_map>

#include "bms/analyze.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

namespace {

struct Symbol_Table {
private:
    using map_type = std::unordered_map<std::string_view, ast::Node_Handle>;
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

    std::variant<map_type::iterator, Node_Handle> emplace(std::string_view symbol,
                                                          ast::Node_Handle node)
    {
        if (m_parent != nullptr) {
            if (std::optional<Node_Handle> old = m_parent->find(symbol)) {
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

    std::optional<Node_Handle> find(std::string_view symbol)
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
    Function_Node* m_current_function = nullptr;

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
    Result<void, Analysis_Error> analyze_symbols_global(Node_Handle handle, T& n) = delete;

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(Node_Handle handle, Program_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);
        for (Node_Handle decl : n.declarations) {
            std::visit(
                [this, decl]<typename T>(T& node) -> Result<void, Analysis_Error> {
                    if constexpr (std::is_same_v<T, Let_Const_Node>
                                  || std::is_same_v<T, Function_Node>) {
                        return analyze_symbols_global(decl, node);
                    }
                    else {
                        BIT_MANIPULATION_ASSERT(false);
                    }
                },
                get_node(decl));
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(Node_Handle handle, Let_Const_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);
        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_global_const, n.token,
                                    get_token(get_node(*old)) };
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error> analyze_symbols_global(Node_Handle handle, Function_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);

        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_function, n.token,
                                    get_token(get_node(*old)) };
        }

        m_current_function = &n;
        return analyze_symbols_local(handle, m_symbols.push(), n);
    }

    Result<void, Analysis_Error> analyze_symbols_local(Node_Handle handle, Symbol_Table& table)
    {
        if (handle == Node_Handle::null) {
            return {};
        }
        return std::visit([this, handle, &table](
                              auto& node) { return analyze_symbols_local(handle, table, node); },
                          get_node(handle));
    }

    Result<void, Analysis_Error> analyze_all_symbols_local(std::span<const Node_Handle> handles,
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
    Result<void, Analysis_Error> analyze_symbols_local(Node_Handle, Symbol_Table& table, T& node)
    {
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(Node_Handle handle, Symbol_Table& table, Parameter_Node& node)
    {
        auto it_or_handle = m_symbols.emplace(node.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_parameter, node.token,
                                    get_token(get_node(*old)) };
        }
        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        Node_Handle g = get_bit_generic_expression(type_node.type);
        if (g == Node_Handle::null) {
            return {};
        }
        if (auto r = analyze_symbols_local(g, table); !r) {
            const auto error = r.error();
            BIT_MANIPULATION_ASSERT(error.code
                                    == Analysis_Error_Code::reference_to_undefined_variable);
            if (auto* id = std::get_if<Id_Expression_Node>(&get_node(g))) {
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
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Block_Statement_Node& node)
    {
        return analyze_all_symbols_local(node.get_children(), table.push());
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Let_Const_Node& node)
    {
        if (std::optional<Node_Handle> old = table.find(node.name)) {
            return Analysis_Error { Analysis_Error_Code::failed_to_define_variable, node.token,
                                    get_token(get_node(*old)) };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Assignment_Node& node)
    {
        if (!table.find(node.name)) {
            return Analysis_Error { Analysis_Error_Code::assignment_of_undefined_variable,
                                    node.token };
        }
        return {};
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Function_Call_Expression_Node& node)
    {
        if (std::optional<Node_Handle> result = table.find(node.function)) {
            node.lookup_result = *result;
            return analyze_all_symbols_local(node.arguments, table);
        }
        return Analysis_Error { Analysis_Error_Code::call_to_undefined_function, node.token };
    }

    template <>
    Result<void, Analysis_Error>
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Id_Expression_Node& node)
    {
        std::string_view name = node.token.extract(m_program.source);
        if (std::optional<Node_Handle> result = table.find(name)) {
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