#include <ranges>

#include "common/variant.hpp"

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"

namespace bit_manipulation::bms {

namespace ast {

Function::Function(const Function& other, Copy_for_Instantiation_Tag)
    : detail::Node_Base(other)
    , detail::Parent<4>(other)
    , m_name(other.m_name)
{
    set_children(other.m_children);
}

} // namespace ast

struct Analyzed_Program::Implementation {

    std::string_view m_file_name;
    std::string_view m_source;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::vector<ast::Some_Node*> m_nodes;
    ast::Some_Node* m_root = nullptr;

    Implementation(const Parsed_Program& program,
                   std::string_view file_name,
                   std::pmr::memory_resource* memory)
        : m_file_name(file_name)
        , m_source(program.get_source())
        , m_memory_resource(memory)
    {
        m_nodes.reserve(program.get_node_count());
        m_root = from_parser_node_recursively(nullptr, program.get_root_handle(), program);
        BIT_MANIPULATION_ASSERT(m_root != nullptr);
    }

    ~Implementation()
    {
        for (ast::Some_Node* const& node : m_nodes) {
            std::destroy_at(node);
        }
    }

    template <typename T, typename... Args>
    [[nodiscard]] ast::Some_Node* emplace(Args&&... args)
    {
        void* storage = m_memory_resource.allocate(sizeof(ast::Some_Node), alignof(ast::Some_Node));
        try {
            auto* result = new (storage) ast::Some_Node(T(std::forward<Args>(args)...));
            m_nodes.push_back(result);
            return result;
        } catch (...) {
            m_memory_resource.deallocate(storage, sizeof(ast::Some_Node), alignof(ast::Some_Node));
            throw;
        }
    }

    [[nodiscard]] ast::Some_Node* from_parser_node_recursively(ast::Some_Node* parent,
                                                               astp::Handle handle,
                                                               const Parsed_Program& parsed);

    template <typename F>
    [[nodiscard]] const ast::Some_Node* find_entity(F filter) const
    {
        BIT_MANIPULATION_ASSERT(m_root);
        const auto& program = get<ast::Program>(*m_root);
        for (const ast::Some_Node* child : program.get_children()) {
            if (visit(filter, *child)) {
                return child;
            }
        }
        return nullptr;
    }

    [[nodiscard]] const ast::Some_Node* find_entity_by_name(std::string_view name) const
    {
        return find_entity([&]<typename N>(const N& node) -> bool {
            if constexpr (std::is_same_v<N, ast::Function> || std::is_same_v<N, ast::Const>) {
                return node.get_name() == name;
            }
            else {
                // global static_assert etc.
                return false;
            }
        });
    }
};

ast::Some_Node*
Analyzed_Program::Implementation::from_parser_node_recursively(ast::Some_Node* parent,
                                                               astp::Handle handle,
                                                               const Parsed_Program& parsed)
{
    if (handle == astp::Handle::null) {
        return nullptr;
    }

    /**
     * Constructs an AST node from a single parser AST node, but not recursively.
     * Note that the various AST nodes have different forms of constructors,
     * so we pick whichever one is appropriate by testing which pattern fits via
     * requires-expression.
     */
    const auto parser_node_to_ast_node = [&]<typename T>(const T& n) -> ast::Some_Node* {
        using Result = typename T::AST_Node;
        if constexpr (requires { Result(n, m_file_name, &m_memory_resource); }) {
            // Root node; everything else should have a parent
            return emplace<Result>(n, m_file_name, &m_memory_resource);
        }
        else if constexpr (requires { Result(*parent, n, m_file_name, &m_memory_resource); }) {
            BIT_MANIPULATION_ASSERT(parent != nullptr);
            // Node which dynamically stores child nodes
            return emplace<Result>(*parent, n, m_file_name, &m_memory_resource);
        }
        else {
            BIT_MANIPULATION_ASSERT(parent != nullptr);
            // Other nodes (static nodes, so to speak)
            return emplace<Result>(*parent, n, m_file_name);
        }
    };

    // Note that the current_parent will be re-assigned within the visitor.
    // This way, we avoid defining the lambda below within another generic lambda,
    // which drastically slows down the linter and probably impacts compilation speed
    // quite negatively.
    ast::Some_Node* current_parent = parent;

    const auto transform_parser_node_child
        = [&](astp::Handle h) { return from_parser_node_recursively(current_parent, h, parsed); };

    return visit(
        [&]<typename T>(const T& n) {
            using Result = typename T::AST_Node;
            ast::Some_Node* result = parser_node_to_ast_node(n);
            current_parent = result;
            get<Result>(*result).set_children(n.get_children()
                                              | std::views::transform(transform_parser_node_child));
            return result;
        },
        parsed.get_node(handle));
}

Analyzed_Program::Analyzed_Program(const Parsed_Program& program,
                                   std::string_view file_name,
                                   std::pmr::memory_resource* memory)
    : m_impl(new Implementation(program, file_name, memory))
{
}

Analyzed_Program::~Analyzed_Program()
{
    delete m_impl;
}

std::string_view Analyzed_Program::get_file_name() const
{
    return m_impl->m_file_name;
}

std::string_view Analyzed_Program::get_source() const
{
    return m_impl->m_source;
}

ast::Some_Node* Analyzed_Program::get_root() const
{
    return m_impl->m_root;
}

std::pmr::memory_resource* Analyzed_Program::get_memory_resource() const
{
    return &m_impl->m_memory_resource;
}

ast::Some_Node* Analyzed_Program::insert(const ast::Some_Node& node)
{
    return m_impl->emplace<ast::Some_Node>(node);
}

ast::Some_Node* Analyzed_Program::insert(ast::Some_Node&& node)
{
    return m_impl->emplace<ast::Some_Node>(node);
}

[[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
Analyzed_Program::find_entity(std::string_view name) const
{
    const ast::Some_Node* entity = m_impl->find_entity_by_name(name);
    if (!entity) {
        return Introspection_Error_Code::nothing_found;
    }
    return entity;
}

[[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
Analyzed_Program::find_global_function_node(std::string_view name) const
{
    const ast::Some_Node* entity = m_impl->find_entity_by_name(name);
    if (!entity) {
        return Introspection_Error_Code::nothing_found;
    }
    if (!holds_alternative<ast::Function>(*entity)) {
        return Introspection_Error_Code::wrong_entity;
    }
    return entity;
}

[[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
Analyzed_Program::find_global_constant_node(std::string_view name) const
{
    const ast::Some_Node* entity = m_impl->find_entity_by_name(name);
    if (!entity) {
        return Introspection_Error_Code::nothing_found;
    }
    if (!holds_alternative<ast::Const>(*entity)) {
        return Introspection_Error_Code::wrong_entity;
    }
    return entity;
}

[[nodiscard]] Result<const ast::Function*, Introspection_Error_Code>
Analyzed_Program::find_global_function(std::string_view name) const
{
    auto node = find_global_function_node(name);
    if (!node) {
        return node.error();
    }
    return &get<ast::Function>(**node);
}

[[nodiscard]] Result<const ast::Const*, Introspection_Error_Code>
Analyzed_Program::find_global_constant(std::string_view name) const
{
    auto node = find_global_constant_node(name);
    if (!node) {
        return node.error();
    }
    return &get<ast::Const>(**node);
}

} // namespace bit_manipulation::bms
