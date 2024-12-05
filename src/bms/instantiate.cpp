#include <memory_resource>
#include <unordered_map>

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/instantiate.hpp"

namespace bit_manipulation::bms {
namespace {

template <typename T>
concept Lookup_Performing_Node = requires(T& t) {
    { t.lookup_result } -> std::same_as<Optional_Lookup_Result&>;
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
    std::pmr::unordered_map<const Parameter*, Parameter*> m_parameter_remap;

public:
    Instantiator(Analyzed_Program& program, std::pmr::memory_resource* memory)
        : m_program(program)
        , m_memory_resource(memory)
        , m_remap(&m_memory_resource)
        , m_parameter_remap(&m_memory_resource)
    {
    }

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

        for (Parameter& p : instance.get_parameters()) {
            ast::Type& type_node = p.get_type();
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

        if (const auto* old_function = get_if<ast::Function>(h)) {
            ast::Some_Node* result = m_program.insert(old_function->copy_for_instantiation());
            m_remap.emplace(h, result);

            auto& instance = get<ast::Function>(*result);
            deep_copy_parameters(*old_function, instance, result);

            ast::Some_Node* return_type
                = deep_copy_for_instantiation(old_function->get_return_type_node());
            instance.set_return_type_node(return_type);

            ast::Some_Node* requires_clause
                = deep_copy_for_instantiation(old_function->get_requires_clause_node());
            instance.set_requires_clause_node(requires_clause);

            ast::Some_Node* body = deep_copy_for_instantiation(old_function->get_body_node());
            instance.set_body_node(body);

            for (auto* child : { return_type, requires_clause, body }) {
                if (child) {
                    set_parent(*child, *result);
                }
            }
            return result;
        }

        ast::Some_Node* result = m_program.insert(*h);
        m_remap.emplace(h, result);
        for (ast::Some_Node*& child : get_children(*result)) {
            child = deep_copy_for_instantiation(child);
            set_parent(*child, *result);
        }
        return result;
    }

    void
    deep_copy_parameters(const ast::Function& from, ast::Function& instance, ast::Some_Node* parent)
    {
        const Size n = from.get_parameter_count();
        BIT_MANIPULATION_ASSERT(instance.get_parameter_count() == n);

        std::span<const Parameter> old_parameters = from.get_parameters();
        std::span<Parameter> new_parameters = instance.get_parameters();

        for (Size i = 0; i < n; ++i) {
            auto [_, success]
                = m_parameter_remap.try_emplace(&old_parameters[i], &new_parameters[i]);
            BIT_MANIPULATION_ASSERT(success);
            ast::Some_Node* type = deep_copy_for_instantiation(old_parameters[i].get_type_node());
            new_parameters[i].set_type_node(type);
            set_parent(*type, *parent);
        }
    }

    struct Remap_Lookup_Result {
        Instantiator& self;
        Optional_Lookup_Result& target;

        void operator()(Monostate) const { }

        void operator()(Builtin_Function) const { }

        void operator()(const ast::Some_Node* looked_up_node)
        {
            target = self.m_remap.at(looked_up_node);
        }

        void operator()(const Parameter* looked_up_parameter)
        {
            target = self.m_parameter_remap.at(looked_up_parameter);
        }
    };

    struct Update_Name_Lookup {
        Instantiator& self;

        void operator()(ast::Id_Expression& n) const
        {
            BIT_MANIPULATION_ASSERT(n.bit_generic || n.lookup_result);
        }
        template <Lookup_Performing_Node T>
        void operator()(T& n)
        {
            visit(Remap_Lookup_Result { self, n.lookup_result }, n.lookup_result);
        }
        void operator()(Ignore) const { }
    };

    void deep_update_name_lookup(ast::Some_Node* h)
    {
        if (h == nullptr) {
            return;
        }
        visit(Update_Name_Lookup { *this }, *h);
        if (auto* function = get_if<ast::Function>(h)) {
            for (Parameter& parameter : function->get_parameters()) {
                deep_update_name_lookup(parameter.get_type_node());
            }
            deep_update_name_lookup(function->get_return_type_node());
            deep_update_name_lookup(function->get_requires_clause_node());
            deep_update_name_lookup(function->get_body_node());
        }
        else {
            for (ast::Some_Node* const child : get_children(*h)) {
                deep_update_name_lookup(child);
            }
        }
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
