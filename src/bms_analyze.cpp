#include <unordered_map>

#include "bms.hpp"

using namespace bit_manipulation::ast;

namespace bit_manipulation {

namespace {

Node_Handle get_bit_generic_expression(Some_Type& type)
{
    if (Bit_Generic_Type* g = std::get_if<Bit_Generic_Type>(&type)) {
        return g->width;
    }
    return Node_Handle::null;
}

Node_Handle get_bit_generic_expression(Type_Node& type)
{
    return get_bit_generic_expression(type.type);
}

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

struct Analyzer {
private:
    Parsed_Program& m_program;
    Program_Node& m_root;
    Symbol_Table m_symbols;

public:
    Analyzer(Parsed_Program& program)
        : m_program(program)
        , m_root(std::get<Program_Node>(program.get_node(program.root_node)))
    {
    }

    Analysis_Result analyze()
    {
        return analyze_symbols_global(m_program.root_node, m_root);
    }

private:
    Some_Node& get_node(Node_Handle handle)
    {
        return m_program.get_node(handle);
    }

    // SYMBOL LOOKUP ANALYSIS ======================================================================

    template <typename T>
    Analysis_Result analyze_symbols_global(Node_Handle handle, T& n) = delete;

    template <>
    Analysis_Result analyze_symbols_global(Node_Handle handle, Program_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);
        for (Node_Handle decl : n.declarations) {
            std::visit(
                [this, decl]<typename T>(T& node) -> Analysis_Result {
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
        return Analysis_Result::ok;
    }

    template <>
    Analysis_Result analyze_symbols_global(Node_Handle handle, Let_Const_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);
        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return { .code = Analysis_Error_Code::failed_to_define_global_const,
                     .fail_token = n.token,
                     .cause_token = get_token(get_node(*old)) };
        }
        return Analysis_Result::ok;
    }

    template <>
    Analysis_Result analyze_symbols_global(Node_Handle handle, Function_Node& n)
    {
        BIT_MANIPULATION_ASSERT(handle != Node_Handle::null);
        auto it_or_handle = m_symbols.emplace(n.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return { .code = Analysis_Error_Code::failed_to_define_function,
                     .fail_token = n.token,
                     .cause_token = get_token(get_node(*old)) };
        }
        return analyze_symbols_local(handle, m_symbols.push(), n);
    }

    Analysis_Result analyze_symbols_local(Node_Handle handle, Symbol_Table& table)
    {
        if (handle == Node_Handle::null) {
            return Analysis_Result::ok;
        }
        return std::visit([this, handle, &table](
                              auto& node) { return analyze_symbols_local(handle, table, node); },
                          get_node(handle));
    }

    Analysis_Result analyze_all_symbols_local(std::span<const Node_Handle> handles,
                                              Symbol_Table& table)
    {
        for (auto h : handles) {
            if (auto r = analyze_symbols_local(h, table); !r) {
                return r;
            }
        }
        return Analysis_Result::ok;
    }

    template <typename T>
    Analysis_Result analyze_symbols_local(Node_Handle, Symbol_Table& table, T& node)
    {
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Analysis_Result
    analyze_symbols_local(Node_Handle handle, Symbol_Table& table, Parameter_Node& node)
    {
        auto it_or_handle = m_symbols.emplace(node.name, handle);
        if (Node_Handle* old = std::get_if<Node_Handle>(&it_or_handle)) {
            return { .code = Analysis_Error_Code::failed_to_define_parameter,
                     .fail_token = node.token,
                     .cause_token = get_token(get_node(*old)) };
        }
        auto& type_node = std::get<Type_Node>(get_node(node.get_type()));
        Node_Handle g = get_bit_generic_expression(type_node);
        if (g == Node_Handle::null) {
            return Analysis_Result::ok;
        }
        if (auto r = analyze_symbols_local(g, table); !r) {
            BIT_MANIPULATION_ASSERT(r.code == Analysis_Error_Code::reference_to_undefined_variable);
            if (auto* id = std::get_if<Id_Expression_Node>(&get_node(g))) {
                table.emplace(r.fail_token.extract(m_program.source), g);
                id->bit_generic = true;
            }
            else {
                return r;
            }
        }
        return Analysis_Result::ok;
    }

    template <>
    Analysis_Result
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Block_Statement_Node& node)
    {
        return analyze_all_symbols_local(node.get_children(), table.push());
    }

    template <>
    Analysis_Result analyze_symbols_local(Node_Handle, Symbol_Table& table, Let_Const_Node& node)
    {
        if (std::optional<Node_Handle> old = table.find(node.name)) {
            return { .code = Analysis_Error_Code::failed_to_define_variable,
                     .fail_token = node.token,
                     .cause_token = get_token(get_node(*old)) };
        }
        return analyze_all_symbols_local(node.get_children(), table);
    }

    template <>
    Analysis_Result analyze_symbols_local(Node_Handle, Symbol_Table& table, Assignment_Node& node)
    {
        if (!table.find(node.name)) {
            return { .code = Analysis_Error_Code::assignment_of_undefined_variable,
                     .fail_token = node.token };
        }
        return Analysis_Result::ok;
    }

    template <>
    Analysis_Result
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Function_Call_Expression_Node& node)
    {
        if (std::optional<Node_Handle> result = table.find(node.function)) {
            node.lookup_result = *result;
            return analyze_all_symbols_local(node.arguments, table);
        }
        return { .code = Analysis_Error_Code::call_to_undefined_function,
                 .fail_token = node.token };
    }

    template <>
    Analysis_Result
    analyze_symbols_local(Node_Handle, Symbol_Table& table, Id_Expression_Node& node)
    {
        std::string_view name = node.token.extract(m_program.source);
        if (std::optional<Node_Handle> result = table.find(name)) {
            node.lookup_result = *result;
            return Analysis_Result::ok;
        }
        return { .code = Analysis_Error_Code::reference_to_undefined_variable,
                 .fail_token = node.token };
    }
};

} // namespace

Analysis_Result analyze(Parsed_Program& program)
{
    Analyzer analyzer { program };
    return analyzer.analyze();
}

} // namespace bit_manipulation