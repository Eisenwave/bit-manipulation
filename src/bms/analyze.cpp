#include <unordered_map>

#include "bms/bms.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

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

struct Analyzer_Base {
    Parsed_Program& m_program;
    Program_Node& m_root;

    Analyzer_Base(Parsed_Program& program)
        : m_program(program)
        , m_root(std::get<Program_Node>(program.get_node(program.root_node)))
    {
    }

    Some_Node& get_node(Node_Handle handle)
    {
        return m_program.get_node(handle);
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

public:
    Name_Lookup_Analyzer(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Analysis_Result operator()()
    {
        return analyze_symbols_global(m_program.root_node, m_root);
    }

private:
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

/// @brief Class responsible for performing instantiations.
/// After runnings this analyzer, functions will have attached instance nodes.
/// These instance nodes are clones of the original function node but where the bit-generic
/// parameter types are replaced with concrete types.
/// Also, the id-expression within the bit-generic type will have a constant value, which
/// facilitates constant propagation in further stages.
struct Instantiator : Analyzer_Base {

    Instantiator(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

    Analysis_Result operator()(int width)
    {
        return instantiate(width);
    }

private:
    Analysis_Result instantiate(int width)
    {
        BIT_MANIPULATION_ASSERT(width > 0);
        BIT_MANIPULATION_ASSERT(width <= uint_max_width);

        for (Node_Handle decl : m_root.declarations) {
            if (auto* f = std::get_if<Function_Node>(&get_node(decl))) {
                Node_Handle instance = clone_node(decl);
                auto& instance_node = std::get<Function_Node>(get_node(instance));
                Analysis_Result r = instantiate(instance_node, width);
                if (!r) {
                    return r;
                }
                f->instances.push_back(instance);
            }
        }

        return Analysis_Result::ok;
    }

    Analysis_Result instantiate([[maybe_unused]] Function_Node& instance,
                                [[maybe_unused]] int width)
    {
        auto& params = std::get<Parameter_List_Node>(get_node(instance.get_parameters()));
        for (Node_Handle p : params.parameters) {
            auto& param_node = std::get<Parameter_Node>(get_node(p));
            auto& type_node = std::get<Type_Node>(get_node(param_node.get_type()));
            if (auto* generic_type = std::get_if<Bit_Generic_Type>(&type_node.type)) {
                BIT_MANIPULATION_ASSERT(generic_type->type == Type_Type::Uint);
                auto& id_node = std::get<Id_Expression_Node>(get_node(generic_type->width));
                BIT_MANIPULATION_ASSERT(id_node.bit_generic);
                // Merely replacing the type isn't enough; we must also give this node a constant
                // value so that existing name lookup can obtain this value.
                id_node.const_value = Value { Concrete_Type::Int, width };
                type_node.type = Concrete_Type::Uint(width);
            }
        }

        return Analysis_Result::ok;
    }

    Node_Handle clone_node(Node_Handle h)
    {
        // Two passes are necessary.
        // During the first pass, the nodes are all copied and the links to children are updated
        // in the copies.
        // However, this does not yet affect the name lookup results, which must also refer to
        // the cloned nodes.
        // This is done in a second pass, since cloning a cyclic graph in one go is difficult.
        std::unordered_map<Node_Handle, Node_Handle> remap;
        const Node_Handle result = clone_node_first_pass(h, remap);
        clone_node_second_pass(h, remap);

        return result;
    }

    Node_Handle clone_node_first_pass(Node_Handle h,
                                      std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        const Node_Handle result
            = std::visit([this]<typename T>(const T& n) { return m_program.push_node(T(n)); },
                         m_program.get_node(h));
        remap.emplace(h, result);

        for (Node_Handle& child : get_children(m_program.get_node(result))) {
            child = clone_node_first_pass(child, remap);
        }
        return result;
    }

    void clone_node_second_pass(Node_Handle h,
                                const std::unordered_map<Node_Handle, Node_Handle>& remap)
    {
        std::visit(
            [this, &h, &remap]<typename T>(T& n) {
                if constexpr (Lookup_Performing_Node<T>) {
                    n.lookup_result = remap.at(h);
                }
                for (Node_Handle child : get_children(n)) {
                    clone_node_second_pass(child, remap);
                }
            },
            m_program.get_node(h));
    }
};

} // namespace

Analysis_Result analyze(Parsed_Program& program)
{
    Name_Lookup_Analyzer analyzer { program };
    return analyzer();
}

} // namespace bit_manipulation::bms