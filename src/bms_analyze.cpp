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

struct Symbol_Table {
private:
    using map_type = std::unordered_map<std::string_view, ast::Node_Handle>;
    map_type symbols;
    Symbol_Table* parent = nullptr;

public:
    Symbol_Table() = default;

    Symbol_Table(Symbol_Table& parent)
        : parent(&parent)
    {
    }

    std::pair<map_type::iterator, bool> emplace(std::string_view symbol, ast::Node_Handle node)
    {
        if (parent != nullptr) {
            if (auto iter = parent->symbols.find(symbol); iter != parent->symbols.end()) {
                return { iter, false };
            }
        }
        return symbols.emplace(symbol, node);
    }

    std::optional<Node_Handle> find(std::string_view symbol)
    {
        if (parent != nullptr) {
            if (auto parent_result = parent->find(symbol)) {
                return parent_result;
            }
        }
        if (auto iter = symbols.find(symbol); iter != symbols.end()) {
            return iter->second;
        }
        return std::nullopt;
    }
};

struct Analyzer {
private:
    Parsed_Program& m_program;
    ast::Node& m_root;
    Symbol_Table m_global_symbols;
    std::unordered_map<std::string_view, Symbol_Table> m_function_tables;

public:
    Analyzer(Parsed_Program& program)
        : m_program(program)
        , m_root(program.get_node(program.root_node))
    {
    }

    void analyze()
    {
        analyze_symbol_lookup();
    }

private:
    Node& get_node(Node_Handle handle)
    {
        return m_program.get_node(handle);
    }

    Analysis_Result analyze_symbol_lookup()
    {
        Program_Data& data = std::get<Program_Data>(m_root.data);
        for (Node_Handle decl : data.declarations) {
            Node& decl_node = get_node(decl);
            if (decl_node.type == Node_Type::variable) {
                auto& d = std::get<Let_Const_Data>(decl_node.data);
                auto [old, success] = m_global_symbols.emplace(d.name, decl);
                if (!success) {
                    return { .code = Analysis_Error_Code::failed_to_define_global_const,
                             .fail_token = decl_node.token,
                             .cause_token = get_node(old->second).token };
                }
            }

            else if (decl_node.type == Node_Type::function) {
                auto& d = std::get<Function_Data>(decl_node.data);
                auto [old, success] = m_global_symbols.emplace(d.name, decl);
                if (!success) {
                    return { .code = Analysis_Error_Code::failed_to_define_function,
                             .fail_token = decl_node.token,
                             .cause_token = get_node(old->second).token };
                }
                auto [table_pos, table_success]
                    = m_function_tables.emplace(d.name, m_global_symbols);
                BIT_MANIPULATION_ASSERT(table_success);
                if (auto r = analyze_function_symbol_lookup(decl, table_pos->second); !r) {
                    return r;
                }
            }

            else {
                BIT_MANIPULATION_ASSERT(false);
            }
        }
        return { Analysis_Error_Code::ok };
    }

    Analysis_Result analyze_function_symbol_lookup(std::span<const Node_Handle> handles,
                                                   Symbol_Table& table)
    {
        for (auto h : handles) {
            if (auto r = analyze_function_symbol_lookup(h, table); !r) {
                return r;
            }
        }
        return { Analysis_Error_Code::ok };
    }

    Analysis_Result analyze_function_symbol_lookup(std::initializer_list<Node_Handle> handles,
                                                   Symbol_Table& table)
    {
        return analyze_function_symbol_lookup(std::span { handles }, table);
    }

    Analysis_Result analyze_function_symbol_lookup(Node_Handle handle, Symbol_Table& table)
    {
        if (handle == Node_Handle::null) {
            return { .code = Analysis_Error_Code::ok };
        }
        Node& n = get_node(handle);

        switch (n.type) {

        case Node_Type::function: {
            // Emplacing the function itself is not necessary because it can already be looked up
            // through the parent.

            auto& data = std::get<Function_Data>(n.data);
            if (auto r = analyze_function_symbol_lookup(data.parameters, table); !r) {
                return r;
            }
            return analyze_function_symbol_lookup(
                { data.return_type, data.requires_clause, data.body }, table);
        }

        case Node_Type::parameter: {
            auto& data = std::get<Parameter_Data>(n.data);
            auto [old, success] = table.emplace(data.name, handle);
            if (!success) {
                return { .code = Analysis_Error_Code::failed_to_define_parameter,
                         .fail_token = n.token,
                         .cause_token = get_node(old->second).token };
            }
            Node_Handle g = get_bit_generic_expression(data.type);
            if (g == Node_Handle::null) {
                return { Analysis_Error_Code::ok };
            }
            if (auto r = analyze_function_symbol_lookup(g, table); !r) {
                BIT_MANIPULATION_ASSERT(r.code
                                        == Analysis_Error_Code::reference_to_undefined_variable);
                if (get_node(g).type == Node_Type::id_expression) {
                    table.emplace(r.fail_token.extract(m_program.source), g);
                }
                else {
                    return r;
                }
            }
            return { Analysis_Error_Code::ok };
        }

        case Node_Type::type: {
            auto& data = std::get<Some_Type>(n.data);
            return analyze_function_symbol_lookup(get_bit_generic_expression(data), table);
        }

        case Node_Type::variable: {
            auto& data = std::get<Let_Const_Data>(n.data);
            if (std::optional<Node_Handle> old = table.find(data.name)) {
                return { .code = Analysis_Error_Code::failed_to_define_variable,
                         .fail_token = n.token,
                         .cause_token = get_node(*old).token };
            }
            return analyze_function_symbol_lookup({ data.type, data.initializer }, table);
        }

        case Node_Type::if_statement: {
            auto& data = std::get<If_Statement_Data>(n.data);
            return analyze_function_symbol_lookup(
                { data.condition, data.if_block, data.else_block }, table);
        }

        case Node_Type::while_statement: {
            auto& data = std::get<While_Statement_Data>(n.data);
            return analyze_function_symbol_lookup({ data.condition, data.block }, table);
        }

        case Node_Type::return_statement: {
            auto& data = std::get<Return_Statement_Data>(n.data);
            return analyze_function_symbol_lookup(data.expression, table);
        }

        case Node_Type::assignment: {
            auto& data = std::get<Assignment_Data>(n.data);
            if (!table.find(data.name)) {
                return { .code = Analysis_Error_Code::assignment_of_undefined_variable,
                         .fail_token = n.token };
            }
            return { Analysis_Error_Code::ok };
        }

        case Node_Type::block_statement: {
            auto& data = std::get<Block_Statement_Data>(n.data);
            return analyze_function_symbol_lookup(data.statements, table);
        }

        case Node_Type::if_expression: {
            auto& data = std::get<If_Expression_Data>(n.data);
            return analyze_function_symbol_lookup({ data.left, data.condition, data.right }, table);
        }

        case Node_Type::binary_expression: {
            auto& data = std::get<Binary_Expression_Data>(n.data);
            return analyze_function_symbol_lookup({ data.left, data.right }, table);
        }

        case Node_Type::prefix_expression: {
            auto& data = std::get<Prefix_Expression_Data>(n.data);
            return analyze_function_symbol_lookup({ data.operand }, table);
        }

        case Node_Type::function_call_expression: {
            auto& data = std::get<Function_Call_Expression_Data>(n.data);
            if (!table.find(data.function)) {
                return { .code = Analysis_Error_Code::call_to_undefined_function,
                         .fail_token = n.token };
            }
            return analyze_function_symbol_lookup(data.arguments, table);
        }

        // TODO: store result of name lookup
        // TODO: make it possible to iterate over children for all nodes, which would eliminate most
        // cases
        case Node_Type::id_expression: {
            std::string_view name = n.token.extract(m_program.source);
            if (std::optional<Node_Handle> old = table.find(name)) {
                n.value = get_node(*old).value;
            }
            else {
                return { .code = Analysis_Error_Code::reference_to_undefined_variable,
                         .fail_token = n.token };
            }
            return { Analysis_Error_Code::ok };
        }

        case Node_Type::literal: {
            return { Analysis_Error_Code::ok };
        }
        }
        // unreachable
    }
};

} // namespace

Analysis_Result analyze(Parsed_Program& program)
{
    Analyzer analyzer { program };
    analyzer.analyze();
}

} // namespace bit_manipulation