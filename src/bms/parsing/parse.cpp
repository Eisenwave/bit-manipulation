#include <functional>
#include <memory_resource>
#include <optional>
#include <vector>

#include "common/assert.hpp"
#include "common/result.hpp"

#include "bms/concrete_type.hpp"
#include "bms/diagnostic_consumer.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/token_type.hpp"

namespace bit_manipulation::bms {

struct Parsed_Program::Implementation {
    std::pmr::vector<astp::Some_Node> nodes;
    std::string_view source;
    astp::Handle root_node = astp::Handle::null;

    explicit Implementation(std::string_view source, std::pmr::memory_resource* memory)
        : nodes(memory)
        , source(source)
    {
    }
};

Parsed_Program::Parsed_Program(std::string_view source, std::pmr::memory_resource* memory)
    : m_memory(memory)
    , m_impl(allocator().new_object<Implementation>(source, memory))
{
}

[[nodiscard]] Parsed_Program::Parsed_Program(Parsed_Program&& other) noexcept
    : m_memory(other.m_memory)
    , m_impl(std::exchange(other.m_impl, nullptr))
{
}

Parsed_Program& Parsed_Program::operator=(Parsed_Program&& other) noexcept
{
    Implementation* old = std::exchange(m_impl, std::exchange(other.m_impl, nullptr));
    if (old) {
        allocator().delete_object(old);
    }
    return *this;
}

Parsed_Program::~Parsed_Program()
{
    if (m_impl) {
        allocator().delete_object(m_impl);
    }
}

/// @brief Return the program source code.
[[nodiscard]] std::string_view Parsed_Program::get_source() const
{
    return m_impl->source;
}

/// @brief Returns a handle to the root node.
[[nodiscard]] astp::Handle Parsed_Program::get_root_handle() const
{
    return m_impl->root_node;
}

void Parsed_Program::set_root_handle(astp::Handle root)
{
    BIT_MANIPULATION_ASSERT(root != astp::Handle::null);
    m_impl->root_node = root;
}

/// @brief Returns the number of parsed AST nodes.
[[nodiscard]] Size Parsed_Program::get_node_count() const
{
    return m_impl->nodes.size();
}

astp::Some_Node& Parsed_Program::get_node(astp::Handle handle) &
{
    BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < m_impl->nodes.size());
    return m_impl->nodes[static_cast<Size>(handle)];
}

const astp::Some_Node& Parsed_Program::get_node(astp::Handle handle) const&
{
    BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < m_impl->nodes.size());
    return m_impl->nodes[static_cast<Size>(handle)];
}

[[nodiscard]] std::string_view Parsed_Program::extract(const Local_Source_Span& span) const
{
    return m_impl->source.substr(span.begin, span.length);
}

void Parsed_Program::downsize_nodes(Size n) &
{
    BIT_MANIPULATION_ASSERT(n <= m_impl->nodes.size());
    m_impl->nodes.erase(m_impl->nodes.begin() + std::ptrdiff_t(n), m_impl->nodes.end());
}

astp::Handle Parsed_Program::push_node(astp::Some_Node&& node) &
{
    const auto result = static_cast<astp::Handle>(m_impl->nodes.size());
    m_impl->nodes.push_back(std::move(node));
    return result;
}

std::pmr::polymorphic_allocator<> Parsed_Program::allocator() const
{
    return std::pmr::polymorphic_allocator<>(m_memory);
}

namespace {

/// @brief Tries to shrink the vector to fit the given size.
/// Unlike `std::vector::resize`, this does not require the type to be default-constructible.
/// @param vec the vector
/// @param size the desired size
/// @return `true` if `size <= vec.size()`, in which case the vector is resized.
template <typename T, typename Alloc>
bool try_downsize(std::vector<T, Alloc>& vec, typename std::vector<T, Alloc>::size_type size)
{
    if (size > vec.size()) {
        return false;
    }
    vec.erase(vec.begin() + static_cast<std::vector<T, Alloc>::difference_type>(size), vec.end());
    return true;
}

} // namespace

namespace astp {

Program::Program(Local_Source_Span pos, std::pmr::vector<astp::Handle>&& declarations)
    : Node_Base { pos }
    , declarations(std::move(declarations))
{
}

Function::Function(Local_Source_Span pos,
                   std::string_view name,
                   Handle annotations,
                   Handle parameters,
                   Handle return_type,
                   Handle requires_clause,
                   Handle body)
    : Node_Base { pos }
    , Parent<5> { annotations, parameters, return_type, requires_clause, body }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(return_type != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(body != astp::Handle::null);
}

Parameter_List::Parameter_List(Local_Source_Span pos, std::pmr::vector<astp::Handle>&& parameters)
    : Node_Base { pos }
    , parameters((std::move(parameters)))
{
    for (auto h : parameters) {
        BIT_MANIPULATION_ASSERT(h != astp::Handle::null);
    }
}

Parameter::Parameter(Local_Source_Span pos, std::string_view name, Handle type)
    : Node_Base { pos }
    , Parent<1> { type }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(type != astp::Handle::null);
}

Type::Type(Local_Source_Span pos, Type_Type type, Handle width)
    : Node_Base { pos }
    , Parent<1> { width }
    , type(type)
{
    BIT_MANIPULATION_ASSERT(type != Type_Type::Uint || width != Handle::null);
}

Const::Const(Local_Source_Span pos,
             std::string_view name,
             Handle annotations,
             Handle type,
             Handle initializer)
    : Node_Base { pos }
    , Parent<3> { annotations, type, initializer }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(initializer != astp::Handle::null);
}

Let::Let(Local_Source_Span pos,
         std::string_view name,
         Handle annotations,
         Handle type,
         Handle initializer)
    : Node_Base { pos }
    , Parent<3> { annotations, type, initializer }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(type != astp::Handle::null || initializer != astp::Handle::null);
}

Static_Assert::Static_Assert(Local_Source_Span pos, Handle expression)
    : Node_Base { pos }
    , Parent<1> { expression }
{
    BIT_MANIPULATION_ASSERT(expression != astp::Handle::null);
}

Annotation_List::Annotation_List(Local_Source_Span pos, std::pmr::vector<Handle>&& annotations)
    : Node_Base { pos }
    , annotations(std::move(annotations))
{
}

Annotation::Annotation(Local_Source_Span pos,
                       std::string_view name,
                       std::pmr::vector<Handle>&& arguments)
    : Node_Base { pos }
    , name { name }
    , arguments(std::move(arguments))
{
}

Annotation_Argument::Annotation_Argument(Local_Source_Span pos,
                                         std::string_view key,
                                         std::string_view value,
                                         Token_Type value_type)
    : Node_Base { pos }
    , key { key }
    , value { value }
    , value_type { value_type }
{
}

If_Statement::If_Statement(Local_Source_Span pos,
                           Handle condition,
                           Handle if_block,
                           Handle else_block)
    : Node_Base { pos }
    , Parent<3> { condition, if_block, else_block }
{
    BIT_MANIPULATION_ASSERT(condition != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(if_block != astp::Handle::null);
}

While_Statement::While_Statement(Local_Source_Span pos, Handle condition, Handle block)
    : Node_Base { pos }
    , Parent<2> { condition, block }
{
    BIT_MANIPULATION_ASSERT(condition != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(block != astp::Handle::null);
}

Break::Break(Local_Source_Span pos)
    : Node_Base { pos }
{
}

Continue::Continue(Local_Source_Span pos)
    : Node_Base { pos }
{
}

Return_Statement::Return_Statement(Local_Source_Span pos, Handle expression)
    : Node_Base { pos }
    , Parent<1> { expression }
{
}

Assignment::Assignment(Local_Source_Span pos,
                       std::string_view name,
                       Handle annotations,
                       Handle expression)
    : Node_Base { pos }
    , Parent<2> { annotations, expression }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(expression != astp::Handle::null);
}

Block_Statement::Block_Statement(Local_Source_Span pos, std::pmr::vector<astp::Handle>&& statements)
    : Node_Base { pos }
    , statements(std::move(statements))
{
}

Conversion_Expression::Conversion_Expression(Local_Source_Span pos,
                                             Handle expression,
                                             Handle target_type)
    : Node_Base { pos }
    , Parent<2> { expression, target_type }
{
    BIT_MANIPULATION_ASSERT(expression != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(target_type != astp::Handle::null);
}

If_Expression::If_Expression(Local_Source_Span pos, Handle left, Handle condition, Handle right)
    : Node_Base { pos }
    , Parent<3> { left, condition, right }
{
    BIT_MANIPULATION_ASSERT(condition != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(left != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(right != astp::Handle::null);
}

Binary_Expression::Binary_Expression(Local_Source_Span pos,
                                     Handle left,
                                     Handle right,
                                     Token_Type op)
    : Node_Base { pos }
    , Parent<2> { left, right }
    , op(op)
{
    BIT_MANIPULATION_ASSERT(left != astp::Handle::null);
    BIT_MANIPULATION_ASSERT(right != astp::Handle::null);
}

Prefix_Expression::Prefix_Expression(Local_Source_Span pos, Token_Type op, Handle operand)
    : Node_Base { pos }
    , Parent<1> { operand }
    , op(op)
{
    BIT_MANIPULATION_ASSERT(operand != astp::Handle::null);
}

Function_Call_Expression::Function_Call_Expression(Local_Source_Span pos,
                                                   std::string_view function,
                                                   std::pmr::vector<astp::Handle>&& arguments,
                                                   bool is_statement)
    : Node_Base { pos }
    , function(function)
    , arguments(std::move(arguments))
    , is_statement(is_statement)
{
}

Id_Expression::Id_Expression(Local_Source_Span pos, std::string_view identifier)
    : Node_Base { pos }
    , identifier(identifier)
{
}

Literal::Literal(Local_Source_Span pos, std::string_view literal, Token_Type type)
    : Node_Base { pos }
    , literal(literal)
    , type(type)
{
}

} // namespace astp

namespace {

template <auto X>
constexpr decltype(X) const_array_one_v[1] = { X };

// Like `Parse_Error`, but without the `token` member because the parser keeps track of the token
// position anyway, and it's more convenient to not deal with it most of the time.
struct Rule_Error {
    Grammar_Rule rule;
    std::span<const Token_Type> expected_tokens;
};

struct Parser {
private:
    /// @brief The result program.
    Parsed_Program& m_program;
    /// @brief The input tokens.
    std::span<const Token> m_tokens;
    /// @brief The parser position within `m_tokens`.
    Size m_pos = 0;

public:
    explicit Parser(Parsed_Program& program, std::span<const Token> tokens)
        : m_program { program }
        , m_tokens { tokens }
    {
    }

    Result<void, Parse_Error> operator()()
    {
        if (auto program = match_program()) {
            m_program.set_root_handle(m_program.push_node(std::move(*program)));
            return {};
        }
        else {
            const auto fail_token = m_pos < m_tokens.size() ? m_tokens[m_pos] : Token {};
            const auto [fail_rule, expected_tokens] = program.error();
            return Parse_Error { fail_rule, expected_tokens, fail_token };
        }
    }

private:
    /// @brief Checks whether the parser is at the end of the file, in a way that respects comments.
    /// @return `true` if the parser has no more tokens to examine.
    [[nodiscard]] bool eof() noexcept
    {
        skip_comments();
        return m_pos == m_tokens.size();
    }

    void skip_comments() noexcept
    {
        while (m_pos != m_tokens.size() && is_comment(m_tokens[m_pos].type)) {
            m_pos += 1;
        }
    }

    template <std::invocable<Token_Type> Predicate>
    const Token* peek_or_expect(Predicate p, bool increment) noexcept
    {
        if (const Token* next = peek(); next && p(next->type)) {
            m_pos += increment;
            return next;
        }
        return nullptr;
    }

    /// @brief Returns the next token if there is one, without advancing the parser.
    /// If there is no next token (end of file), `nullptr` is returned.
    /// @return The next token or `nullptr`.
    [[nodiscard]] const Token* peek() noexcept
    {
        return !eof() ? &m_tokens[m_pos] : nullptr;
    }

    /// @brief Returns the n-th next token if there is one, without advancing the parser.
    /// `peek_n(0)` is equivalent to `peek()`.
    /// @param lookahead the amount of nodes to look ahead
    /// @return The n-th next token or `nullptr`.
    [[nodiscard]] const Token* peek_n(Size lookahead) noexcept
    {
        const Size restore = m_pos;
        const Token* result;
        for (Size i = 0; i <= lookahead; ++i, ++m_pos) {
            result = peek();
            if (!result) {
                break;
            }
        }
        m_pos = restore;
        return result;
    }

    /// @return `peek() && peek() == expected ? peek() : nullptr`.
    const Token* peek(Token_Type expected) noexcept
    {
        return peek_or_expect([=](Token_Type t) { return t == expected; }, false);
    }

    /// @return `peek() && predicate(peek()->type) ? peek() : nullptr`.
    const Token* peek(bool (&predicate)(Token_Type)) noexcept
    {
        return peek_or_expect(predicate, false);
    }

    /// @brief Checks whether the next token (if any) equals the expected type.
    /// If so, the parser advances by one token.
    /// Otherwise, does nothing and returns `nullptr`.
    /// @param type the expected type
    /// @return The popped token with the given type, or `nullptr` if there is no token, or the
    /// token doesn't match the expected type.
    const Token* expect(Token_Type type) noexcept
    {
        return peek_or_expect([=](Token_Type t) { return t == type; }, true);
    }

    const Token* expect(bool predicate(Token_Type)) noexcept
    {
        return peek_or_expect(predicate, true);
    }

    using Rule_Result = Result<astp::Some_Node, Rule_Error>;

    struct Scoped_Attempt {
        Parser& self;
        const Size pos;
        const Size node_count;

        explicit Scoped_Attempt(Parser& self)
            : self(self)
            , pos(self.m_pos)
            , node_count(self.m_program.get_node_count())
        {
        }

        ~Scoped_Attempt() noexcept(false)
        {
            BIT_MANIPULATION_ASSERT(self.m_pos >= pos);
            BIT_MANIPULATION_ASSERT(self.m_program.get_node_count() >= node_count);
            self.m_pos = pos;
            self.m_program.downsize_nodes(node_count);
        }
    };

    Scoped_Attempt start_attempt()
    {
        return Scoped_Attempt { *this };
    }

    [[maybe_unused]] Result<std::optional<astp::Some_Node>, Rule_Error>
    optionally_match(Rule_Result (Parser::*match)(), bool condition)
    {
        BIT_MANIPULATION_ASSERT(match);
        if (!condition) {
            return std::optional<astp::Some_Node> {};
        }
        auto result = (this->*match)();
        if (!result) {
            return result.error();
        }
        return std::optional<astp::Some_Node> { std::move(*result) };
    }

    /// @brief Pushes the node and returns the corresponding handle if the node has a value,
    /// otherwise returns `null`.
    [[maybe_unused]] [[nodiscard]] astp::Handle push_or_null(std::optional<astp::Some_Node>&& node)
    {
        return node ? m_program.push_node(std::move(*node)) : astp::Handle::null;
    }

    /// @brief Pushes the node and returns the corresponding handle if the node is not null,
    /// otherwise returns `null`.
    [[nodiscard]] astp::Handle push_or_null(astp::Some_Node* node)
    {
        return node ? m_program.push_node(std::move(*node)) : astp::Handle::null;
    }

    Rule_Result match_program()
    {
        auto first = match_program_declaration();
        if (!first) {
            return first;
        }
        std::pmr::vector<astp::Handle> declarations(m_program.get_memory());
        declarations.push_back(m_program.push_node(std::move(*first)));

        while (!eof()) {
            auto d = match_program_declaration();
            if (!d) {
                return d;
            }
            declarations.push_back(m_program.push_node(std::move(*d)));
        }
        return astp::Some_Node { astp::Program { get_source_position(*first),
                                                 std::move(declarations) } };
    }

    Rule_Result match_program_declaration()
    {
        constexpr auto this_rule = Grammar_Rule::program_declaration;
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_const, Token_Type::keyword_function,
                Token_Type::keyword_static_assert, Token_Type::at };

        const Token* next = peek();
        if (!next) {
            return Rule_Error { this_rule, expected };
        }

        if (next->type == Token_Type::at) {
            auto annotations = match_annotation_sequence();
            if (!annotations) {
                return annotations;
            }
            return match_annotated_program_declaration(*annotations);
        }

        switch (next->type) {
        case Token_Type::keyword_const: return match_const_declaration();
        case Token_Type::keyword_function: return match_function_declaration();
        case Token_Type::keyword_static_assert: return match_static_assertion();
        default: break;
        }

        return Rule_Error { this_rule, expected };
    }

    Rule_Result match_annotated_program_declaration(astp::Some_Node& annotations)
    {
        constexpr auto this_rule = Grammar_Rule::program_declaration;
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_const, Token_Type::keyword_function };

        if (peek(Token_Type::keyword_const)) {
            return match_const_declaration(&annotations);
        }
        if (peek(Token_Type::keyword_function)) {
            return match_function_declaration(&annotations);
        }
        return Rule_Error { this_rule, expected };
    }

    Rule_Result match_let_declaration(astp::Some_Node* annotations = nullptr)
    {
        const auto this_rule = Grammar_Rule::let_declaration;

        const Token* t = expect(Token_Type::keyword_let);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_let> };
        }
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        const std::string_view name = m_program.extract(id->pos);

        // TODO: refactor to match_and_push_if
        auto type_handle = astp::Handle::null;
        if (expect(Token_Type::colon)) {
            if (auto type = match_type()) {
                type_handle = m_program.push_node(std::move(*type));
            }
            else {
                return type;
            }
        }
        // TODO: refactor to match_and_push_if
        auto init_handle = astp::Handle::null;
        if (type_handle == astp::Handle::null || peek(Token_Type::assign)) {
            auto init = match_initializer();
            if (!init) {
                return init;
            }
            init_handle = m_program.push_node(std::move(*init));
        }

        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Let { t->pos, name, push_or_null(annotations), type_handle,
                                             init_handle } };
    }

    Rule_Result match_const_declaration(astp::Some_Node* annotations = nullptr)
    {
        const auto this_rule = Grammar_Rule::const_declaration;

        const Token* t = expect(Token_Type::keyword_const);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_const> };
        }
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        const std::string_view name = m_program.extract(id->pos);

        // TODO: refactor with match_and_push_if
        auto type_handle = astp::Handle::null;
        if (expect(Token_Type::colon)) {
            if (auto type = match_type()) {
                type_handle = m_program.push_node(std::move(*type));
            }
            else {
                return type;
            }
        }
        auto init = match_initializer();
        if (!init) {
            return init;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Const { t->pos, name, push_or_null(annotations), type_handle,
                                               m_program.push_node(std::move(*init)) } };
    }

    Rule_Result match_initializer()
    {
        constexpr auto this_rule = Grammar_Rule::initializer;
        if (!expect(Token_Type::assign)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
        }
        return match_expression();
    }

    Rule_Result match_function_declaration(astp::Some_Node* annotations = nullptr)
    {
        constexpr auto this_rule = Grammar_Rule::function_declaration;

        const Token* t = expect(Token_Type::keyword_function);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_function> };
        }
        const Token* name = expect(Token_Type::identifier);
        if (!name) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::left_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }

        // TODO: refactor to match_and_push_if
        auto parameters = astp::Handle::null;
        if (!peek(Token_Type::right_parenthesis)) {
            // By first checking whether there is no right parenthesis, we can ensure that there
            // must be parameters.
            // This is not strictly necessary, but leads to improved diagnostics because we can
            // commit to parsing the parameters.
            auto r = match_parameter_sequence();
            if (!r) {
                return r;
            }
            parameters = m_program.push_node(std::move(*r));
        }

        if (!expect(Token_Type::right_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
        }
        if (!expect(Token_Type::right_arrow)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::right_arrow> };
        }
        auto ret = match_type();
        if (!ret) {
            return ret;
        }
        // TODO: refactor to match_and_push_if
        auto requires_handle = astp::Handle::null;
        if (peek(Token_Type::keyword_requires)) {
            if (auto req = match_requires_clause()) {
                requires_handle = m_program.push_node(std::move(*req));
            }
            else {
                return req;
            }
        }
        auto body = match_block_statement();
        if (!body) {
            return body;
        }
        return astp::Some_Node { astp::Function {
            t->pos, m_program.extract(name->pos), //
            push_or_null(annotations), parameters, m_program.push_node(std::move(*ret)),
            requires_handle, m_program.push_node(std::move(*body)) } };
    }

    Rule_Result match_parameter_sequence()
    {
        Local_Source_Span first_pos;
        std::pmr::vector<astp::Handle> parameters(m_program.get_memory());
        while (true) {
            auto p = match_parameter();
            if (!p) {
                return p;
            }
            first_pos = get<astp::Parameter>(*p).pos;
            parameters.push_back(m_program.push_node(std::move(*p)));
            if (!expect(Token_Type::comma)) {
                break;
            }
        }
        BIT_MANIPULATION_ASSERT(!parameters.empty());

        return astp::Some_Node { astp::Parameter_List { first_pos, std::move(parameters) } };
    }

    Rule_Result match_parameter()
    {
        constexpr auto this_rule = Grammar_Rule::parameter;
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::colon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::colon> };
        }
        auto type = match_type();
        if (!type) {
            return type;
        }
        return astp::Some_Node { astp::Parameter { id->pos, m_program.extract(id->pos),
                                                   m_program.push_node(std::move(*type)) } };
    }

    Rule_Result match_static_assertion()
    {
        constexpr auto this_rule = Grammar_Rule::static_assertion;
        const Token* t = expect(Token_Type::keyword_static_assert);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_static_assert> };
        }
        auto expression = match_parenthesized_expression();
        if (!expression) {
            return expression;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Static_Assert {
            t->pos, m_program.push_node(std::move(*expression)) } };
    }

    Rule_Result match_requires_clause()
    {
        constexpr auto this_rule = Grammar_Rule::requires_clause;
        const Token* t = expect(Token_Type::keyword_requires);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_requires> };
        }
        return match_expression();
    }

    Rule_Result match_annotation_sequence()
    {
        Local_Source_Span first_pos;
        std::pmr::vector<astp::Handle> annotations(m_program.get_memory());
        while (true) {
            auto annotation = match_annotation();
            if (!annotation) {
                return annotation;
            }
            first_pos = get<astp::Annotation>(*annotation).pos;
            annotations.push_back(m_program.push_node(std::move(*annotation)));
            if (!peek(Token_Type::at)) {
                break;
            }
        }
        BIT_MANIPULATION_ASSERT(!annotations.empty());

        return astp::Some_Node { astp::Annotation_List { first_pos, std::move(annotations) } };
    }

    Rule_Result match_annotation()
    {
        constexpr auto this_rule = Grammar_Rule::annotation;
        const Token* at = expect(Token_Type::at);
        if (!at) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::at> };
        }
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }

        auto arguments = [&]() -> Result<std::pmr::vector<astp::Handle>, Rule_Error> {
            if (!expect(Token_Type::left_parenthesis) || expect(Token_Type::right_parenthesis)) {
                return std::pmr::vector<astp::Handle>(m_program.get_memory());
            }
            auto r = match_annotation_argument_sequence();
            if (r && !expect(Token_Type::right_parenthesis)) {
                return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
            }
            return r;
        }();
        return astp::Some_Node { astp::Annotation { at->pos, m_program.extract(id->pos),
                                                    std::move(*arguments) } };
    }

    Result<std::pmr::vector<astp::Handle>, Rule_Error> match_annotation_argument_sequence()
    {
        std::pmr::vector<astp::Handle> arguments(m_program.get_memory());
        while (true) {
            auto argument = match_annotation_argument();
            if (!argument) {
                return argument.error();
            }
            arguments.push_back(m_program.push_node(std::move(*argument)));
            if (!expect(Token_Type::comma)) {
                break;
            }
        }
        BIT_MANIPULATION_ASSERT(!arguments.empty());

        return arguments;
    }

    Rule_Result match_annotation_argument()
    {
        constexpr auto this_rule = Grammar_Rule::annotation_argument;
        static constexpr Token_Type expected[]
            = { Token_Type::identifier,          Token_Type::binary_literal,
                Token_Type::octal_literal,       Token_Type::decimal_literal,
                Token_Type::hexadecimal_literal, Token_Type::string_literal,
                Token_Type::keyword_true,        Token_Type::keyword_false };
        static constexpr auto expected_after_key = std::span { expected }.subspan(1);

        const Token* key = expect(Token_Type::identifier);
        if (key) {
            if (!expect(Token_Type::assign)) {
                return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
            }
        }
        const std::string_view key_string = key ? m_program.extract(key->pos) : "";

        const Token* value = expect(is_literal);
        if (!value) {
            return Rule_Error { this_rule, key ? expected_after_key : expected };
        }
        const Local_Source_Span pos = key ? key->pos : value->pos;
        const std::string_view value_string = m_program.extract(value->pos);

        return astp::Some_Node { astp::Annotation_Argument { pos, key_string, value_string,
                                                             value->type } };
    }

    Rule_Result match_statement()
    {
        constexpr auto this_rule = Grammar_Rule::statement;
        // This is a manually computed FIRST set of the statement rule.
        static constexpr Token_Type possible_types[] = { Token_Type::keyword_let,
                                                         Token_Type::keyword_const,
                                                         Token_Type::keyword_static_assert,
                                                         Token_Type::keyword_break,
                                                         Token_Type::keyword_continue,
                                                         Token_Type::keyword_return,
                                                         Token_Type::keyword_if,
                                                         Token_Type::keyword_while,
                                                         Token_Type::left_brace,
                                                         Token_Type::at,
                                                         Token_Type::identifier };

        const Token* next = peek();
        if (!next) {
            return Rule_Error { this_rule, possible_types };
        }

        if (next->type == Token_Type::at) {
            auto annotations = match_annotation_sequence();
            if (!annotations) {
                return annotations;
            }
            return match_annotated_statement(*annotations);
        }

        switch (next->type) {
        case Token_Type::keyword_let: return match_let_declaration();
        case Token_Type::keyword_const: return match_const_declaration();
        case Token_Type::keyword_static_assert: return match_static_assertion();
        case Token_Type::keyword_break: return match_break_statement();
        case Token_Type::keyword_continue: return match_continue_statement();
        case Token_Type::keyword_return: return match_return_statement();
        case Token_Type::keyword_if: return match_if_statement();
        case Token_Type::keyword_while: return match_while_statement();
        case Token_Type::left_brace: return match_block_statement();
        case Token_Type::identifier: return match_assignment_or_function_call_statement();
        default: break;
        }

        return Rule_Error { this_rule, possible_types };
    }

    Rule_Result match_assignment_or_function_call_statement()
    {
        constexpr auto this_rule = Grammar_Rule::statement;
        if (const Token* t = peek(); !t || t->type != Token_Type::identifier) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }

        if (const Token* lookahead = peek_n(1);
            lookahead && lookahead->type == Token_Type::assign) {
            return match_assignment_statement();
        }
        auto r = match_function_call_statement();
        if (r) {
            get<astp::Function_Call_Expression>(*r).is_statement = true;
        }
        return r;
    }

    Rule_Result match_annotated_statement(astp::Some_Node& annotations)
    {
        constexpr auto this_rule = Grammar_Rule::statement;
        static constexpr Token_Type possible_types[]
            = { Token_Type::keyword_let, Token_Type::keyword_const, Token_Type::identifier };

        if (const Token* next = peek()) {
            switch (next->type) {
            case Token_Type::keyword_const: return match_const_declaration(&annotations);
            case Token_Type::keyword_let: return match_let_declaration(&annotations);
            case Token_Type::identifier: return match_assignment_statement(&annotations);
            default: break;
            }
        }

        return Rule_Error { this_rule, possible_types };
    }

    Rule_Result match_assignment_statement(astp::Some_Node* annotations = nullptr)
    {
        constexpr auto this_rule = Grammar_Rule::assignment_statement;
        auto a = match_assignment(annotations);
        if (!a) {
            return a;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return a;
    }

    Rule_Result match_function_call_statement()
    {
        constexpr auto this_rule = Grammar_Rule::function_call_statement;
        auto call = match_function_call_expression();
        if (!call) {
            return call;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return call;
    }

    Rule_Result match_assignment(astp::Some_Node* annotations = nullptr)
    {
        constexpr auto this_rule = Grammar_Rule::assignment;

        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::assign)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
        }
        auto e = match_expression();
        if (!e) {
            return e;
        }
        return astp::Some_Node { astp::Assignment { id->pos, m_program.extract(id->pos),
                                                    push_or_null(annotations),
                                                    m_program.push_node(std::move(*e)) } };
    }

    Rule_Result match_return_statement()
    {
        constexpr auto this_rule = Grammar_Rule::return_statement;
        const Token* t = expect(Token_Type::keyword_return);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_return> };
        }
        if (expect(Token_Type::semicolon)) {
            return astp::Some_Node { astp::Return_Statement { t->pos, astp::Handle::null } };
        }
        auto e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Return_Statement { t->pos,
                                                          m_program.push_node(std::move(*e)) } };
    }

    Rule_Result match_break_statement()
    {
        constexpr auto this_rule = Grammar_Rule::break_statement;
        const Token* t = expect(Token_Type::keyword_break);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_break> };
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Break { t->pos } };
    }

    Rule_Result match_continue_statement()
    {
        constexpr auto this_rule = Grammar_Rule::continue_statement;
        const Token* t = expect(Token_Type::keyword_continue);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_continue> };
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return astp::Some_Node { astp::Continue { t->pos } };
    }

    Rule_Result match_if_statement()
    {
        const auto this_rule = Grammar_Rule::if_statement;

        const Token* first = expect(Token_Type::keyword_if);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_if> };
        }
        auto condition = match_expression();
        if (!condition) {
            return condition;
        }
        auto block = match_block_statement();
        if (!block) {
            return block;
        }
        auto else_handle = astp::Handle::null;
        if (peek(Token_Type::keyword_else)) {
            auto r = match_else_statement();
            if (!r) {
                return r.error();
            }
            else_handle = m_program.push_node(std::move(*r));
        }
        return astp::Some_Node { astp::If_Statement {
            first->pos, m_program.push_node(std::move(*condition)),
            m_program.push_node(std::move(*block)), else_handle } };
    }

    Rule_Result match_else_statement()
    {
        const auto this_rule = Grammar_Rule::else_statement;

        const Token* first = expect(Token_Type::keyword_else);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_else> };
        }
        return peek(Token_Type::keyword_if) ? match_if_statement() : match_block_statement();
    }

    Rule_Result match_while_statement()
    {
        const auto this_rule = Grammar_Rule::while_statement;

        const Token* first = expect(Token_Type::keyword_while);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_while> };
        }
        auto condition = match_expression();
        if (!condition) {
            return condition;
        }
        auto block = match_block_statement();
        if (!block) {
            return block;
        }
        return astp::Some_Node { astp::While_Statement { first->pos,
                                                         m_program.push_node(std::move(*condition)),
                                                         m_program.push_node(std::move(*block)) } };
    }

    Rule_Result match_block_statement()
    {
        constexpr auto this_rule = Grammar_Rule::block_statement;
        const Token* first = expect(Token_Type::left_brace);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_brace> };
        }
        std::pmr::vector<astp::Handle> statements(m_program.get_memory());
        while (true) {
            if (expect(Token_Type::right_brace)) {
                return astp::Some_Node { astp::Block_Statement { first->pos,
                                                                 std::move(statements) } };
            }
            else if (auto s = match_statement()) {
                statements.push_back(m_program.push_node(std::move(*s)));
            }
            else {
                return s;
            }
        }
        BIT_MANIPULATION_UNREACHABLE();
    }

    Rule_Result match_expression()
    {
        bool is_conversion_expression = [&]() {
            Scoped_Attempt always_roll_back = start_attempt();
            return match_prefix_expression() && peek(Token_Type::keyword_as);
        }();

        return is_conversion_expression ? match_conversion_expression() //
                                        : match_if_expression();
    }

    Rule_Result match_conversion_expression()
    {
        constexpr auto this_rule = Grammar_Rule::conversion_expression;
        auto expression = match_prefix_expression();
        if (!expression) {
            return expression;
        }
        if (!expect(Token_Type::keyword_as)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_as> };
        }
        auto type = match_type();
        if (!type) {
            return type;
        }
        return astp::Some_Node { astp::Conversion_Expression {
            get_source_position(*expression), m_program.push_node(std::move(*expression)),
            m_program.push_node(std::move(*type)) } };
    }

    Rule_Result match_if_expression()
    {
        constexpr auto this_rule = Grammar_Rule::if_expression;
        auto left = match_binary_expression();
        if (!left || !expect(Token_Type::keyword_if)) {
            return left;
        }
        auto condition = match_binary_expression();
        if (!condition) {
            return condition;
        }
        if (!expect(Token_Type::keyword_else)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_else> };
        }
        auto right = match_if_expression();
        if (!right) {
            return right;
        }
        return astp::Some_Node { astp::If_Expression {
            get_source_position(*left), m_program.push_node(std::move(*left)),
            m_program.push_node(std::move(*condition)), m_program.push_node(std::move(*right)) } };
    }

    Rule_Result match_binary_expression()
    {
        // FIXME: use a similar roll_back_after approach to match_conversion_expression
        if (should_binary_expression_commit_to_comparison_expression()) {
            return match_comparison_expression();
        }

        auto left = match_prefix_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_binary_operator);
        if (!op) {
            return left;
        }
        auto right = match_prefix_expression();
        if (!right) {
            return right;
        }
        return astp::Some_Node { astp::Binary_Expression {
            op->pos, m_program.push_node(std::move(*left)), m_program.push_node(std::move(*right)),
            op->type } };
    }

#if BIT_MANIPULATION_ENABLE_EXPERIMENTAL_PARSING
    Rule_Result match_binary_expression_impl(astp::Some_Node lhs,
                                             Binary_Operator_Precedence min_precedence)
    {
        constexpr auto this_rule = Grammar_Rule::statement;
#if 0
        static constexpr Token_Type expected[]
            = { Token_Type::equals,        Token_Type::not_equals,       Token_Type::plus,
                Token_Type::minus,         Token_Type::multiplication,   Token_Type::division,
                Token_Type::remainder,     Token_Type::less_than,        Token_Type::greater_than,
                Token_Type::less_or_equal, Token_Type::greater_or_equal, Token_Type::shift_left,
                Token_Type::shift_right,   Token_Type::bitwise_and,      Token_Type::bitwise_or,
                Token_Type::logical_and,   Token_Type::logical_or };
#endif
        // The following implementation is conceptually based on
        // https://en.wikipedia.org/wiki/Operator-precedence_parser

        const Token* lookahead = peek(is_binary_operator);
        while (lookahead) {
            if (binary_operator_precedence_of(lookahead->type) < min_precedence) {
                break;
            }
            const Token* const op = lookahead;
            const auto op_precedence = binary_operator_precedence_of(op->type);

            ++m_pos;
            auto rhs = match_prefix_expression();
            if (!rhs) {
                return rhs;
            }

            lookahead = peek(is_binary_operator);
            if (lookahead && //
                binary_operator_precedence_of(lookahead->type) > op_precedence) {

                const auto rhs_precedence = ++binary_operator_precedence_of(op->type);
                rhs = match_binary_expression_impl(std::move(*rhs), rhs_precedence);
                if (!rhs) {
                    return rhs;
                }
                lookahead = peek(is_binary_operator);
            }
            lhs = astp::Binary_Expression { op->pos, //
                                            m_program.push_node(std::move(lhs)),
                                            m_program.push_node(std::move(*rhs)), op->type };
            // This effectively disables operator chaining.
            // I.e. (a + b + c) is disallowed; we only match (a + b).
            // However, (a + b == c) is allowed, so if we increase
            if (lookahead && binary_operator_precedence_of(lookahead->type) == min_precedence) {
                break;
            }
        }
        return lhs;
    }
#endif

    /// @brief An arbitrary look-ahead member function which tells us whether we should commit
    /// to matching a comparison expression when matching a binary expression.
    ///
    /// Consider for example that if we are parsing `(0) == 3`, we can already tell that this
    /// needs to be a comparison expression based on the following `==`.
    /// However, identifying this situation is not as simple as looking at the next token due to
    /// arbitrary amounts of parentheses.
    ///
    /// This approach is a bit unusual, but makes sense for BMS specifically.
    /// Thanks to this test, our parser is completely deterministic and we can avoid more complex
    /// expression parsing approach.
    /// @return `true` if we should commit, `false` otherwise.
    bool should_binary_expression_commit_to_comparison_expression() const
    {
        Size parenthesis_level = 0;
        for (Size i = m_pos; i < m_tokens.size(); ++i) {
            switch (m_tokens[i].type) {
            case Token_Type::left_brace:
            case Token_Type::right_brace:
            case Token_Type::semicolon: return false;

            case Token_Type::left_parenthesis: ++parenthesis_level; break;
            case Token_Type::right_parenthesis: --parenthesis_level; break;

            default:
                if (parenthesis_level != 0) {
                    continue;
                }
                if (m_tokens[i].type == Token_Type::keyword_if) {
                    return false;
                }
                if (is_comparison_operator(m_tokens[i].type)) {
                    return true;
                }
            }
        }
        return false;
    }

    Rule_Result match_comparison_expression()
    {
        constexpr auto this_rule = Grammar_Rule::comparison_expression;
        static constexpr Token_Type expected[]
            = { Token_Type::equals,       Token_Type::not_equals,    Token_Type::less_than,
                Token_Type::greater_than, Token_Type::less_or_equal, Token_Type::greater_or_equal };

        auto left = match_arithmetic_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_comparison_operator);
        if (!op) {
            return Rule_Error { this_rule, expected };
        }
        auto right = match_arithmetic_expression();
        if (!right) {
            return right;
        }
        return astp::Some_Node { astp::Binary_Expression {
            op->pos, m_program.push_node(std::move(*left)), m_program.push_node(std::move(*right)),
            op->type } };
    }

    Rule_Result match_arithmetic_expression()
    {
        auto left = match_prefix_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_arithmetic_operator);
        if (!op) {
            return left;
        }
        auto right = match_prefix_expression();
        if (!right) {
            return right;
        }
        return astp::Some_Node { astp::Binary_Expression {
            op->pos, m_program.push_node(std::move(*left)), m_program.push_node(std::move(*right)),
            op->type } };
    }

    Rule_Result match_prefix_expression()
    {
        if (const Token* t = expect(is_unary_operator)) {
            auto e = match_postfix_expression();
            if (!e) {
                return e;
            }
            return astp::Some_Node { astp::Prefix_Expression {
                t->pos, t->type, m_program.push_node(std::move(*e)) } };
        }
        return match_postfix_expression();
    }

    Rule_Result match_postfix_expression()
    {
        if (peek(Token_Type::identifier)) {
            if (const Token* lookahead = peek_n(1);
                lookahead && lookahead->type == Token_Type::left_parenthesis) {
                return match_function_call_expression();
            }
        }
        return match_primary_expression();
    }

    Rule_Result match_function_call_expression()
    {
        constexpr auto this_rule = Grammar_Rule::function_call_expression;
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::left_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }

        std::pmr::vector<astp::Handle> arguments(m_program.get_memory());
        for (bool demand_expression = false; true;) {
            if (!demand_expression && expect(Token_Type::right_parenthesis)) {
                break;
            }
            auto arg = match_expression();
            if (!arg) {
                return arg;
            }
            arguments.push_back(m_program.push_node(std::move(*arg)));
            demand_expression = expect(Token_Type::comma);
        }
        return astp::Some_Node { astp::Function_Call_Expression {
            id->pos, m_program.extract(id->pos), std::move(arguments) } };
    }

    Rule_Result match_primary_expression()
    {
        // FIXME: boolean literals don't appear to be supported, but should be matched here
        constexpr auto this_rule = Grammar_Rule::primary_expression;
        static constexpr Token_Type expected[]
            = { Token_Type::binary_literal,  Token_Type::octal_literal,
                Token_Type::decimal_literal, Token_Type::hexadecimal_literal,
                Token_Type::identifier,      Token_Type::left_parenthesis };

        if (const Token* t = expect(is_literal)) {
            return astp::Some_Node { astp::Literal { t->pos, m_program.extract(t->pos), t->type } };
        }
        if (const Token* t = expect(Token_Type::identifier)) {
            return astp::Some_Node { astp::Id_Expression { t->pos, m_program.extract(t->pos) } };
        }
        if (auto e = match_parenthesized_expression()) {
            return e;
        }
        // Intentionally forget about the e error to not falsely suggest that parentheses are
        // required for a primary expression, but give feedback about the whole union.
        return Rule_Error { this_rule, expected };
    }

    [[maybe_unused]] Rule_Result match_parenthesized_expression()
    {
        constexpr auto this_rule = Grammar_Rule::parenthesized_expression;
        const Token* t = expect(Token_Type::left_parenthesis);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }
        auto e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::right_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
        }
        return e;
    }

    Rule_Result match_type()
    {
        constexpr auto this_rule = Grammar_Rule::type;
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_void, Token_Type::keyword_bool, Token_Type::keyword_int,
                Token_Type::keyword_uint };

        if (const Token* t = expect(Token_Type::keyword_void)) {
            return astp::Some_Node { astp::Type { t->pos, Type_Type::Void, astp::Handle::null } };
        }
        if (const Token* t = expect(Token_Type::keyword_bool)) {
            return astp::Some_Node { astp::Type { t->pos, Type_Type::Bool, astp::Handle::null } };
        }
        if (const Token* t = expect(Token_Type::keyword_int)) {
            return astp::Some_Node { astp::Type { t->pos, Type_Type::Int, astp::Handle::null } };
        }
        if (const Token* t = expect(Token_Type::keyword_uint)) {
            auto e = match_parenthesized_expression();
            if (!e) {
                return e;
            }
            return astp::Some_Node { astp::Type { t->pos, Type_Type::Uint,
                                                  m_program.push_node(std::move(*e)) } };
        }

        return Rule_Error { this_rule, expected };
    }
};

} // namespace

Result<void, Parse_Error> parse(Parsed_Program& program, std::span<const Token> tokens)
{
    return Parser { program, tokens }();
}

bool parse(Parsed_Program& program, std::span<const Token> tokens, Diagnostic_Consumer& diagnostics)
{
    if (auto result = parse(program, tokens)) {
        return true;
    }
    else {
        diagnostics(std::move(result.error()));
        return false;
    }
}

} // namespace bit_manipulation::bms
