#include <ranges>

#include "common/variant.hpp"

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"

namespace bit_manipulation::bms {

namespace ast {

namespace detail {

Node_Base::Node_Base()
    : m_parent(nullptr)
    , m_const_value(Value::unknown_of_type(Concrete_Type::Void))
{
}

Node_Base::Node_Base(Root_Node_Tag, const astp::detail::Node_Base& parsed, std::string_view file)
    : m_parent(nullptr)
    , m_position(Source_Span { parsed.pos, file })
{
}

Node_Base::Node_Base(Some_Node& parent,
                     const astp::detail::Node_Base& parsed,
                     std::string_view file)
    : m_parent(&parent)
    , m_position(Source_Span { parsed.pos, file })
{
}

Node_Base::Node_Base(Some_Node& parent, const Source_Span& pos, std::optional<Value> value)
    : m_parent(&parent)
    , m_position(pos)
    , m_const_value(value)
{
}

} // namespace detail

Program::Program(const astp::Program& parsed,
                 std::string_view file,
                 std::pmr::memory_resource* memory)
    : detail::Node_Base(Root_Node_Tag {}, parsed, file)
    , detail::Dynamic_Parent(memory)
{
}

Function::Function(Some_Node& parent, const astp::Function& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_name(parsed.name)
{
}

Function::Function(const Function& other, Copy_for_Instantiation_Tag)
    : detail::Node_Base(other)
    , detail::Parent<4>(other)
    , m_name(other.m_name)
{
    set_children(other.m_children);
}

Parameter_List::Parameter_List(Some_Node& parent,
                               const astp::Parameter_List& parsed,
                               std::string_view file,
                               std::pmr::memory_resource* memory)
    : detail::Node_Base(parent, parsed, file)
    , detail::Dynamic_Parent(memory)
{
}

Parameter::Parameter(Some_Node& parent, const astp::Parameter& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_name(parsed.name)
{
}

Type::Type(Some_Node& parent, const astp::Type& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_type(parsed.type)
{
}

Const::Const(Some_Node& parent, const astp::Const& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_name(parsed.name)
{
}

Let::Let(Some_Node& parent, const astp::Let& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_name(parsed.name)
{
}

Static_Assert::Static_Assert(Some_Node& parent,
                             const astp::Static_Assert& parsed,
                             std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

If_Statement::If_Statement(Some_Node& parent,
                           const astp::If_Statement& parsed,
                           std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

While_Statement::While_Statement(Some_Node& parent,
                                 const astp::While_Statement& parsed,
                                 std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

Break::Break(Some_Node& parent, const astp::Break& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

Continue::Continue(Some_Node& parent, const astp::Continue& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

Return_Statement::Return_Statement(Some_Node& parent,
                                   const astp::Return_Statement& parsed,
                                   std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

Assignment::Assignment(Some_Node& parent, const astp::Assignment& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , name(parsed.name)
{
}

Block_Statement::Block_Statement(Some_Node& parent,
                                 const astp::Block_Statement& parsed,
                                 std::string_view file,
                                 std::pmr::memory_resource* memory)
    : detail::Node_Base(parent, parsed, file)
    , detail::Dynamic_Parent(memory)
{
}

Conversion_Expression::Conversion_Expression(Some_Node& parent,
                                             const astp::Conversion_Expression& parsed,
                                             std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

If_Expression::If_Expression(Some_Node& parent,
                             const astp::If_Expression& parsed,
                             std::string_view file)
    : detail::Node_Base(parent, parsed, file)
{
}

Binary_Expression::Binary_Expression(Some_Node& parent,
                                     const astp::Binary_Expression& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_op(parsed.op)
{
}

Prefix_Expression::Prefix_Expression(Some_Node& parent,
                                     const astp::Prefix_Expression& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_op(parsed.op)
{
}

Function_Call_Expression::Function_Call_Expression(Some_Node& parent,
                                                   const astp::Function_Call_Expression& parsed,
                                                   std::string_view file,
                                                   std::pmr::memory_resource* memory)
    : detail::Node_Base(parent, parsed, file)
    , detail::Dynamic_Parent(memory)
    , m_name(parsed.function)
    , m_is_statement(parsed.is_statement)
{
}

Id_Expression::Id_Expression(Some_Node& parent,
                             const astp::Id_Expression& parsed,
                             std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_identifier(parsed.identifier)
{
}

Literal::Literal(Some_Node& parent, const astp::Literal& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_literal(parsed.literal)
    , m_type(parsed.type)
{
}

Builtin_Function::Builtin_Function(bms::Builtin_Function function)
    : detail::Node_Base(detail::Node_Base::make_builtin())
    , m_function(function)
{
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
