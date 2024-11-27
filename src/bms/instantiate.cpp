#include <memory_resource>
#include <unordered_map>

#include "bms/analyze.hpp"
#include "bms/ast.hpp"

namespace bit_manipulation::bms {
namespace {

template <typename T>
concept Lookup_Performing_Node = requires(T& t) {
    { t.lookup_result } -> std::same_as<ast::Some_Node*&>;
};

static_assert(Lookup_Performing_Node<ast::Id_Expression>);

/// @brief Class responsible for performing instantiations.
/// After runnings this analyzer, functions will have attached instance nodes.
/// These instance nodes are clones of the original function node but where the bit-generic
/// parameter types are replaced with concrete types.
/// Also, the id-expression within the bit-generic type will have a constant value, which
/// facilitates constant propagation in further stages.
struct Instantiator {
private:
    Analyzed_Program& m_program;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::unordered_map<const ast::Some_Node*, ast::Some_Node*> m_remap;

public:
    Instantiator(Analyzed_Program& program, std::pmr::memory_resource* memory)
        : m_program(program)
        , m_memory_resource(memory)
        , m_remap(&m_memory_resource)
    {
    }

    /// @brief Instantiates every bit-generic function in the program using the same `Widths`.
    /// This is mostly useful for testing purposes and cannot be triggered "naturally" through
    /// implicit instantiations.
    Result<void, Analysis_Error> instantiate_all(const Widths& w)
    {
        for (ast::Some_Node* decl : get_children(*m_program.get_root())) {
            if (auto* f = get_if<ast::Function>(decl)) {
                auto r = instantiate_function(decl, *f, w);
                if (!r) {
                    return r.error();
                }
            }
        }
        return {};
    }

    /// @brief Instantiates an individual function.
    /// Contrary to `instantiate_all`, this may be necessitated by implicit instantiations,
    /// for example when a function taking `Uint(N)` is called.
    ///
    /// At a high level, the process of instantiation works as follows:
    ///   1. Make a `deep_copy` of the node to be instantiated
    ///   2. `substitute_widths` to replace occurrences of bit-generic parameters with constants
    ///   3. Append the resulting `ast::Function::Instance` to the existing node
    /// @param h the pointer to the function node
    /// @param node the function node
    /// @param w the widths for instantiation
    /// @return the instantiated instance, or `Analysis_Error`
    Result<const ast::Function::Instance*, Analysis_Error>
    instantiate_function(ast::Some_Node* h, ast::Function& node, const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(node.is_generic);

        if (const ast::Function::Instance* const existing = node.find_instance(w)) {
            return existing;
        }

        ast::Some_Node* instance = deep_copy(h);
        auto& instance_node = get<ast::Function>(*instance);
        Result<std::pmr::vector<int>, Analysis_Error> effective_widths
            = substitute_widths(instance_node, w);
        if (!effective_widths) {
            return effective_widths.error();
        }
        return &node.instances.emplace_back(std::move(*effective_widths), instance);
    }

private:
    Result<std::pmr::vector<int>, Analysis_Error> substitute_widths(ast::Function& instance,
                                                                    const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(!instance.is_generic);

        std::pmr::vector<int> widths(m_program.get_memory_resource());
        const auto next_width = [&widths, &w] {
            const int result = get_width(w, widths.size());
            widths.push_back(result);
            return result;
        };

        for (ast::Some_Node* p : instance.get_parameter_nodes()) {
            auto& param_node = get<ast::Parameter>(*p);
            ast::Type& type_node = param_node.get_type();
            if (type_node.get_width_node() == nullptr) {
                continue;
            }
            BIT_MANIPULATION_ASSERT(type_node.get_type() == Type_Type::Uint);
            auto* id_node = get_if<ast::Id_Expression>(type_node.get_width_node());
            // If it isn't an id-expression, it cannot be a generic parameter.
            if (id_node && id_node->bit_generic) {
                const int width = next_width();
                // Merely replacing the type isn't enough; we must also give this node a
                // constant value so that existing name lookup can obtain this value.
                id_node->const_value() = Value::Int(width);
                type_node.concrete_width = width;
            }
        }

        return widths;
    }

    ast::Some_Node* deep_copy(const ast::Some_Node* h)
    {
        // Two passes are necessary.
        // During the first pass, the nodes are all copied and the links to children are updated
        // in the copies.
        // However, this does not yet affect the name lookup results, which must also refer to
        // the cloned nodes.
        // This is done in a second pass, since cloning a cyclic graph in one go is difficult.

        m_remap.clear();
        ast::Some_Node* const result = deep_copy_for_instantiation(h);
        deep_update_name_lookup(result);

        return result;
    }

    ast::Some_Node* deep_copy_for_instantiation(const ast::Some_Node* h)
    {
        if (h == nullptr) {
            return nullptr;
        }
        ast::Some_Node* const result = copy_single_node_for_instantiation(h);
        m_remap.emplace(h, result);

        for (ast::Some_Node*& child : get_children(*result)) {
            child = deep_copy_for_instantiation(child);
        }
        return result;
    }

    void deep_update_name_lookup(ast::Some_Node* h)
    {
        if (h == nullptr) {
            return;
        }
        visit(
            [&]<typename T>(T& n) {
                if constexpr (std::is_same_v<T, ast::Id_Expression>) {
                    BIT_MANIPULATION_ASSERT(n.bit_generic || n.lookup_result != nullptr);
                }
                if constexpr (Lookup_Performing_Node<T>) {
                    if (n.lookup_result != nullptr) {
                        n.lookup_result = m_remap.at(n.lookup_result);
                    }
                }
            },
            *h);
        for (ast::Some_Node* const child : get_children(*h)) {
            deep_update_name_lookup(child);
        }
    }

    ast::Some_Node* copy_single_node_for_instantiation(const ast::Some_Node* h)
    {
        BIT_MANIPULATION_ASSERT(h != nullptr);
        return visit(
            [this]<typename T>(const T& n) {
                if constexpr (requires { n.copy_for_instantiation(); }) {
                    return m_program.insert(n.copy_for_instantiation());
                }
                else {
                    return m_program.insert(n);
                }
            },
            *h);
    }
};

} // namespace

Result<void, Analysis_Error>
instantiate_all(Analyzed_Program& program, std::pmr::memory_resource* memory, const Widths& w)
{
    return Instantiator { program, memory }.instantiate_all(w);
}

Result<const ast::Function::Instance*, Analysis_Error>
instantiate_function(Analyzed_Program& program,
                     std::pmr::memory_resource* memory,
                     ast::Some_Node* h,
                     ast::Function& node,
                     const Widths& w)
{
    return Instantiator { program, memory }.instantiate_function(h, node, w);
}

} // namespace bit_manipulation::bms
