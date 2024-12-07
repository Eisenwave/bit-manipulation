#include <ranges>

#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/debug_info.hpp"
#include "bms/evaluation/builtin_function.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/vm/vm.hpp"

namespace bit_manipulation::bms {

namespace {

[[nodiscard]] Expression_Type binary_expression_type_of_token(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case logical_and: return Expression_Type::logical_and;
    case logical_or: return Expression_Type::logical_or;
    case equals: return Expression_Type::equals;
    case not_equals: return Expression_Type::not_equals;
    case less_than: return Expression_Type::less_than;
    case greater_than: return Expression_Type::greater_than;
    case less_or_equal: return Expression_Type::less_or_equal;
    case plus: return Expression_Type::binary_plus;
    case minus: return Expression_Type::binary_minus;
    case multiplication: return Expression_Type::multiplication;
    case division: return Expression_Type::division;
    case remainder: return Expression_Type::remainder;
    case shift_left: return Expression_Type::shift_left;
    case shift_right: return Expression_Type::shift_right;
    case bitwise_and: return Expression_Type::bitwise_and;
    case bitwise_or: return Expression_Type::bitwise_or;
    case bitwise_xor: return Expression_Type::bitwise_xor;
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid operation.");
    }
}

[[nodiscard]] Expression_Type unary_expression_type_of_token(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case plus: return Expression_Type::unary_plus;
    case minus: return Expression_Type::unary_minus;
    case logical_not: return Expression_Type::logical_not;
    case bitwise_not: return Expression_Type::bitwise_not;
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid m_op.");
    }
}

} // namespace

Parameter::Parameter(const astp::Parameter& parsed, std::string_view file)
    : m_name(parsed.name)
    , m_position(Source_Span { parsed.pos, file })
{
}

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

Function::Function(Some_Node& parent,
                   const astp::Function& parsed,
                   std::string_view file,
                   std::pmr::memory_resource* memory,
                   std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
    , m_name(parsed.name)
    , m_parameters(memory)
{
}

Function::Function(const Function& other, Copy_for_Instantiation_Tag)
    : detail::Node_Base(other)
    , detail::Annotated(other)
    , m_name(other.m_name)
    , m_parameters(other.m_parameters)
    , m_return_type(other.m_return_type)
    , m_requires_clause(other.m_requires_clause)
    , m_body(other.m_body)
{
}

Function::Instance::Instance(std::pmr::vector<int>&& widths, Some_Node* handle)
    : widths(std::move(widths))
    , handle(handle)
{
    BIT_MANIPULATION_ASSERT(handle);
    BIT_MANIPULATION_ASSERT(holds_alternative<Function>(*handle));
}

Type::Type(Some_Node& parent, const astp::Type& parsed, std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_type(parsed.type)
{
}

Const::Const(Some_Node& parent,
             const astp::Const& parsed,
             std::string_view file,
             std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
    , m_name(parsed.name)
{
}

Let::Let(Some_Node& parent,
         const astp::Let& parsed,
         std::string_view file,
         std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
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
                           std::string_view file,
                           std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
{
}

While_Statement::While_Statement(Some_Node& parent,
                                 const astp::While_Statement& parsed,
                                 std::string_view file,
                                 std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
{
}

Control_Statement::Control_Statement(Some_Node& parent,
                                     const astp::Break& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_type(Control_Statement_Type::break_)
{
}

Control_Statement::Control_Statement(Some_Node& parent,
                                     const astp::Continue& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_type(Control_Statement_Type::continue_)
{
}

Control_Statement::Control_Statement(Some_Node& parent,
                                     const astp::Return_Statement& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_type(Control_Statement_Type::return_)
{
}

Assignment::Assignment(Some_Node& parent,
                       const astp::Assignment& parsed,
                       std::string_view file,
                       std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
    , m_name(parsed.name)
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
    , m_type(binary_expression_type_of_token(parsed.op))
{
}

Prefix_Expression::Prefix_Expression(Some_Node& parent,
                                     const astp::Prefix_Expression& parsed,
                                     std::string_view file)
    : detail::Node_Base(parent, parsed, file)
    , m_op(parsed.op)
    , m_type(unary_expression_type_of_token(parsed.op))
{
}

Function_Call_Expression::Function_Call_Expression(Some_Node& parent,
                                                   const astp::Function_Call_Expression& parsed,
                                                   std::string_view file,
                                                   std::pmr::memory_resource* memory,
                                                   std::span<const astp::Handle> annotations)
    : detail::Node_Base(parent, parsed, file)
    , detail::Annotated(detail::Annotations(annotations))
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

} // namespace ast

struct Analyzed_Program::Implementation {

    std::string_view m_file_name;
    std::string_view m_source;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::vector<ast::Some_Node*> m_nodes { &m_memory_resource };
    Virtual_Machine m_vm { &m_memory_resource };

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
        requires requires(Args&&... args) { ast::Some_Node(T(std::forward<Args>(args)...)); }
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

    struct From_Parser_Node;
};

template <typename T, typename... Us>
concept one_of = (... || std::same_as<T, Us>);

struct Analyzed_Program::Implementation::From_Parser_Node {
    Analyzed_Program::Implementation& self;
    const Parsed_Program& parsed;
    const astp::Handle handle;
    ast::Some_Node* const parent;

private:
    ast::Some_Node* transform_recursively(ast::Some_Node* new_parent, astp::Handle h) const
    {
        return h == astp::Handle::null
            ? nullptr
            : visit(From_Parser_Node { self, parsed, h, new_parent }, parsed.get_node(h));
    }

    auto recursive_child_transform(ast::Some_Node* new_parent) const
    {
        return std::views::transform(
            [&, new_parent](astp::Handle h) { return transform_recursively(new_parent, h); });
    }

    template <typename T>
    ast::Some_Node* transform_all_children_recursively(ast::Some_Node* new_parent,
                                                       std::span<const astp::Handle> children) const
    {
        get<T>(*new_parent).set_children(children | recursive_child_transform(new_parent));
        return new_parent;
    }

    std::span<const astp::Handle> get_annotations(astp::Handle list_handle) const
    {
        if (list_handle == astp::Handle::null) {
            return {};
        }
        return get<astp::Annotation_List>(parsed.get_node(list_handle)).get_children();
    }

public:
    ast::Some_Node* operator()(const astp::Program& n) const
    {
        ast::Some_Node* result
            = self.emplace<ast::Program>(n, self.m_file_name, &self.m_memory_resource);
        return transform_all_children_recursively<ast::Program>(result, n.get_children());
    }

    template <one_of<astp::Break, astp::Continue> T>
    ast::Some_Node* operator()(const T& n) const
    {
        return self.emplace<ast::Control_Statement>(*parent, n, self.m_file_name);
    }

    ast::Some_Node* operator()(const astp::Function& n) const
    {
        using Result = ast::Function;
        ast::Some_Node* result
            = self.emplace<Result>(*parent, n, self.m_file_name, &self.m_memory_resource,
                                   get_annotations(n.get_annotations()));

        auto& function = get<Result>(*result);

        std::pmr::vector<Parameter>& parameters = function.get_parameters();
        BIT_MANIPULATION_ASSERT(parameters.empty());

        if (n.get_parameters() != astp::Handle::null) {
            const auto& parsed_parameters
                = get<astp::Parameter_List>(parsed.get_node(n.get_parameters()));
            parameters.reserve(parsed_parameters.get_children().size());

            for (const astp::Handle p : parsed_parameters.get_children()) {
                auto& parameter = get<astp::Parameter>(parsed.get_node(p));
                ast::Some_Node* type = transform_recursively(result, parameter.get_type());
                auto& node = parameters.emplace_back(Parameter { parameter, self.m_file_name });
                node.set_type_node(type);
            }
        }

        ast::Some_Node* return_type = transform_recursively(result, n.get_return_type());
        ast::Some_Node* requires_clause = transform_recursively(result, n.get_requires_clause());
        ast::Some_Node* body = transform_recursively(result, n.get_body());

        function.set_return_type_node(return_type);
        function.set_requires_clause_node(requires_clause);
        function.set_body_node(body);

        return result;
    }

    template <one_of<astp::Type,
                     astp::Static_Assert,
                     astp::Return_Statement,
                     astp::Conversion_Expression,
                     astp::If_Expression,
                     astp::Binary_Expression,
                     astp::Prefix_Expression,
                     astp::Id_Expression,
                     astp::Literal> T>
    ast::Some_Node* operator()(const T& n) const
    {
        using Result = T::AST_Node;
        ast::Some_Node* result = self.emplace<Result>(*parent, n, self.m_file_name);
        return transform_all_children_recursively<Result>(result, n.get_children());
    }

    template <
        one_of<astp::Const, astp::Let, astp::If_Statement, astp::While_Statement, astp::Assignment>
            T>
    ast::Some_Node* operator()(const T& n) const
    {
        // Parser AST nodes always have the annotation list as the first child.
        // The actual translation from the parser annotations to whatever the Result needs happens
        // in the individual constructors, so all we have to do is chop off one child at the start.
        using Result = T::AST_Node;
        ast::Some_Node* result = self.emplace<Result>(*parent, n, self.m_file_name,
                                                      get_annotations(n.get_annotations()));
        return transform_all_children_recursively<Result>(result, n.get_children().subspan(1));
    }

    ast::Some_Node* operator()(const astp::Block_Statement& n) const
    {
        using Result = ast::Block_Statement;
        ast::Some_Node* result
            = self.emplace<Result>(*parent, n, self.m_file_name, &self.m_memory_resource);
        return transform_all_children_recursively<Result>(result, n.get_children());
    }

    ast::Some_Node* operator()(const astp::Function_Call_Expression& n) const
    {
        using Result = ast::Function_Call_Expression;
        ast::Some_Node* result
            = self.emplace<Result>(*parent, n, self.m_file_name, &self.m_memory_resource,
                                   get_annotations(n.get_annotations()));
        return transform_all_children_recursively<Result>(result, n.get_arguments());
    }

    template <one_of<astp::Parameter_List,
                     astp::Parameter,
                     astp::Annotation,
                     astp::Annotation_List,
                     astp::Annotation_Argument> T>
    ast::Some_Node* operator()(const T&) const
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Attributes should not be processed here.");
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
    return visit(From_Parser_Node { *this, parsed, handle, parent }, parsed.get_node(handle));
}

Analyzed_Program::Analyzed_Program(const Parsed_Program& program,
                                   std::string_view file_name,
                                   std::pmr::memory_resource* memory)
    : m_memory(memory)
    , m_impl(allocator().new_object<Implementation>(program, file_name, memory))
{
}

Analyzed_Program::Analyzed_Program(Analyzed_Program&& other) noexcept
    : m_memory(other.m_memory)
    , m_impl(std::exchange(other.m_impl, nullptr))
{
}

Analyzed_Program& Analyzed_Program::operator=(Analyzed_Program&& other) noexcept
{
    Implementation* old = std::exchange(m_impl, std::exchange(other.m_impl, nullptr));
    if (old) {
        allocator().delete_object(old);
    }
    return *this;
}

Analyzed_Program::~Analyzed_Program()
{
    if (m_impl) {
        allocator().delete_object(m_impl);
    }
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

Virtual_Machine& Analyzed_Program::get_vm()
{
    return m_impl->m_vm;
}

ast::Some_Node* Analyzed_Program::insert(const ast::Some_Node& node)
{
    return m_impl->emplace<ast::Some_Node>(node);
}

ast::Some_Node* Analyzed_Program::insert(ast::Some_Node&& node)
{
    return m_impl->emplace<ast::Some_Node>(std::move(node));
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

std::pmr::polymorphic_allocator<> Analyzed_Program::allocator() const
{
    BIT_MANIPULATION_ASSERT(m_memory);
    return std::pmr::polymorphic_allocator<>(m_memory);
}

namespace {

struct Lookup_Result_Debug_Info {

    Debug_Info operator()(const ast::Some_Node* node) const
    {
        return Debug_Info { node };
    }

    Debug_Info operator()(const Parameter* parameter) const
    {
        return parameter->get_debug_info();
    }

    Debug_Info operator()(Builtin_Function builtin) const
    {
        return Debug_Info { Construct::builtin_function, {}, builtin_function_name(builtin) };
    }
};

} // namespace

Debug_Info Analysis_Error_Builder::debug_info_from_parameter(const Parameter& parameter)
{
    return parameter.get_debug_info();
}

Debug_Info Analysis_Error_Builder::debug_info_from_lookup_result(const Lookup_Result& result)
{
    return visit(Lookup_Result_Debug_Info {}, result);
}

Debug_Info::Debug_Info(const ast::Some_Node* node)
    : Debug_Info(bms::ast::get_debug_info(*node))
{
}

} // namespace bit_manipulation::bms
