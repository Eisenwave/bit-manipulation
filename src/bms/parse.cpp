#include <optional>
#include <variant>

#include "assert.hpp"
#include "result.hpp"

#include "bms/parse.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

namespace {

/// @brief Tries to shrink the vector to fit the given size.
/// Unlike `std::vector::resize`, this does not require the type to be default-constructible.
/// @tparam T the element type
/// @param vec the vector
/// @param size the desired size
/// @return `true` if `size <= vec.size()`, in which case the vector is resized.
template <typename T>
bool try_downsize(std::vector<T>& vec, typename std::vector<T>::size_type size)
{
    if (size > vec.size()) {
        return false;
    }
    vec.erase(vec.begin() + static_cast<std::vector<T>::difference_type>(size), vec.end());
    return true;
}

} // namespace

// =================================================================================================

Program_Node::Program_Node(Token token, std::vector<Node_Handle>&& declarations)
    : Node_Base { token }
    , declarations(std::move(declarations))
{
}

Function_Node::Function_Node(Token token,
                             std::string_view name,
                             Node_Handle parameters,
                             Node_Handle return_type,
                             Node_Handle requires_clause,
                             Node_Handle body)
    : Node_Base { token }
    , Parent<4> { parameters, return_type, requires_clause, body }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(return_type != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(body != Node_Handle::null);
}

Parameter_List_Node::Parameter_List_Node(Token token, std::vector<Node_Handle>&& parameters)
    : Node_Base { token }
    , parameters((std::move(parameters)))
{
    for (auto h : parameters) {
        BIT_MANIPULATION_ASSERT(h != Node_Handle::null);
    }
}

Parameter_Node::Parameter_Node(Token token, std::string_view name, Node_Handle type)
    : Node_Base { token }
    , Parent<1> { type }
    , name(name)
{
}

Type_Node::Type_Node(Token token, Some_Type type)
    : Node_Base { token }
    , type(type)
{
}

Const_Node::Const_Node(Token token,
                       std::string_view name,
                       Node_Handle type,
                       Node_Handle initializer)
    : Node_Base { token }
    , Parent<2> { type, initializer }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(initializer != Node_Handle::null);
}

Let_Node::Let_Node(Token token, std::string_view name, Node_Handle type, Node_Handle initializer)
    : Node_Base { token }
    , Parent<2> { type, initializer }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(type != Node_Handle::null || initializer != Node_Handle::null);
}

Static_Assert_Node::Static_Assert_Node(Token token, Node_Handle expression)
    : Node_Base { token }
    , Parent<1> { expression }
{
    BIT_MANIPULATION_ASSERT(expression != Node_Handle::null);
}

If_Statement_Node::If_Statement_Node(Token token,
                                     Node_Handle condition,
                                     Node_Handle if_block,
                                     Node_Handle else_block)
    : Node_Base { token }
    , Parent<3> { condition, if_block, else_block }
{
    BIT_MANIPULATION_ASSERT(condition != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(if_block != Node_Handle::null);
}

While_Statement_Node::While_Statement_Node(Token token, Node_Handle condition, Node_Handle block)
    : Node_Base { token }
    , Parent<2> { condition, block }
{
    BIT_MANIPULATION_ASSERT(condition != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(block != Node_Handle::null);
}

Jump_Node::Jump_Node(Token token)
    : Node_Base { token }
{
}

Return_Statement_Node::Return_Statement_Node(Token token, Node_Handle expression)
    : Node_Base { token }
    , Parent<1> { expression }
{
    BIT_MANIPULATION_ASSERT(expression != Node_Handle::null);
}

Assignment_Node::Assignment_Node(Token token, std::string_view name, Node_Handle expression)
    : Node_Base { token }
    , Parent<1> { expression }
    , name(name)
{
    BIT_MANIPULATION_ASSERT(expression != Node_Handle::null);
}

Block_Statement_Node::Block_Statement_Node(Token token, std::vector<Node_Handle>&& statements)
    : Node_Base { token }
    , statements(std::move(statements))
{
}

If_Expression_Node::If_Expression_Node(Token token,
                                       Node_Handle left,
                                       Node_Handle condition,
                                       Node_Handle right)
    : Node_Base { token }
    , Parent<3> { left, condition, right }
{
    BIT_MANIPULATION_ASSERT(condition != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(left != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(right != Node_Handle::null);
}

Binary_Expression_Node::Binary_Expression_Node(Token token,
                                               Node_Handle left,
                                               Node_Handle right,
                                               Token_Type op)
    : Node_Base { token }
    , Parent<2> { left, right }
    , op(op)
{
    BIT_MANIPULATION_ASSERT(left != Node_Handle::null);
    BIT_MANIPULATION_ASSERT(right != Node_Handle::null);
}

Prefix_Expression_Node::Prefix_Expression_Node(Token token, Token_Type op, Node_Handle operand)
    : Node_Base { token }
    , Parent<1> { operand }
    , op(op)
{
    BIT_MANIPULATION_ASSERT(operand != Node_Handle::null);
}

Function_Call_Expression_Node::Function_Call_Expression_Node(Token token,
                                                             std::string_view function,
                                                             std::vector<Node_Handle>&& arguments)
    : Node_Base { token }
    , function(function)
    , arguments(std::move(arguments))
{
}

Id_Expression_Node::Id_Expression_Node(Token token)
    : Node_Base { token }
{
}

Literal_Node::Literal_Node(Token token)
    : Node_Base { token }
{
}

// =================================================================================================

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
    std::span<const Token> m_tokens;
    Size m_pos;
    Parsed_Program m_program;

public:
    explicit Parser(std::span<const Token> tokens, std::string_view source)
        : m_tokens { tokens }
        , m_pos { 0 }
        , m_program { {}, source, Node_Handle::null }
    {
    }

    Result<Parsed_Program, Parse_Error> parse()
    {
        if (auto program = match_program()) {
            m_program.root_node = *program;
            return std::move(m_program);
        }
        else {
            const auto fail_token = m_pos < m_tokens.size() ? m_tokens[m_pos] : Token {};
            const auto [fail_rule, expected_tokens] = program.error();
            return Parse_Error { fail_rule, expected_tokens, fail_token };
        }
    }

private:
    bool eof() noexcept
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

    const Token* peek() noexcept
    {
        return !eof() ? &m_tokens[m_pos] : nullptr;
    }

    const Token* peek(Token_Type expected) noexcept
    {
        return peek_or_expect([=](Token_Type t) { return t == expected; }, false);
    }

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

    const Token* expect(bool (&predicate)(Token_Type)) noexcept
    {
        return peek_or_expect(predicate, true);
    }

    /// @brief Like `match`, but the parser state is not advanced if no match was made.
    /// @param rule the grammar rule
    /// @return the matched result, or `rule`
    Result<Node_Handle, Rule_Error> expect(Result<Node_Handle, Rule_Error> (Parser::*match)())
    {
        const Size restore_pos = m_pos;
        const Size restore_nodes = m_program.nodes.size();
        auto result = (this->*match)();
        if (!result) {
            // We couldn't have gone backwards in the program.
            BIT_MANIPULATION_ASSERT(m_pos >= restore_pos);
            m_pos = restore_pos;
            // We couldn't have dropped any nodes.
            const bool is_downsize = try_downsize(m_program.nodes, restore_nodes);
            BIT_MANIPULATION_ASSERT(is_downsize);
        }
        return result;
    }

    Result<Node_Handle, Rule_Error> match_program()
    {
        auto first = match_program_declaration();
        if (!first) {
            return first;
        }
        std::vector<Node_Handle> declarations;
        declarations.push_back(*first);

        while (!eof()) {
            auto d = match_program_declaration();
            if (!d) {
                return d;
            }
            declarations.push_back(*d);
        }
        return m_program.push_node(
            Program_Node { get_token(m_program.get_node(*first)), std::move(declarations) });
    }

    Result<Node_Handle, Rule_Error> match_program_declaration()
    {
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_const, Token_Type::keyword_function,
                Token_Type::keyword_static_assert };

        if (peek(Token_Type::keyword_const)) {
            return match_const_declaration();
        }
        if (peek(Token_Type::keyword_function)) {
            return match_function_declaration();
        }
        if (peek(Token_Type::keyword_static_assert)) {
            return match_static_assertion();
        }
        return Rule_Error { Grammar_Rule::program_declaration, expected };
    }

    Result<Node_Handle, Rule_Error> match_let_declaration()
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
        const std::string_view name = id->extract(m_program.source);

        auto type_handle = Node_Handle::null;
        if (expect(Token_Type::colon)) {
            if (auto type = match_type()) {
                type_handle = *type;
            }
            else {
                return type;
            }
        }
        auto init = expect(&Parser::match_initializer);
        if (!init && type_handle == Node_Handle::null) {
            return init;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return m_program.push_node(Let_Node { *t, name, type_handle, *init });
    }

    Result<Node_Handle, Rule_Error> match_const_declaration()
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
        const std::string_view name = id->extract(m_program.source);

        auto type_handle = Node_Handle::null;
        if (expect(Token_Type::colon)) {
            if (auto type = match_type()) {
                type_handle = *type;
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
        return m_program.push_node(Const_Node { *t, name, type_handle, *init });
    }

    Result<Node_Handle, Rule_Error> match_initializer()
    {
        constexpr auto this_rule = Grammar_Rule::initializer;
        if (!expect(Token_Type::assign)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
        }
        auto e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return e;
    }

    Result<Node_Handle, Rule_Error> match_function_declaration()
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

        Node_Handle parameters = Node_Handle::null;
        if (!peek(Token_Type::right_parenthesis)) {
            // By first checking whether there is no right parenthesis, we can ensure that there
            // must be parameters.
            // This is not strictly necessary, but leads to improved diagnostics because we can
            // commit to parsing the parameters.
            auto r = match_parameter_sequence();
            if (!r) {
                return r;
            }
            parameters = *r;
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
        auto requires_handle = Node_Handle::null;
        if (peek(Token_Type::keyword_requires)) {
            if (auto req = match_requires_clause()) {
                requires_handle = *req;
            }
            else {
                return req;
            }
        }
        auto body = match_block_statement();
        if (!body) {
            return body;
        }
        return m_program.push_node(Function_Node { *t, name->extract(m_program.source),
                                                   std::move(parameters), *ret, requires_handle,
                                                   *body });
    }

    Result<Node_Handle, Rule_Error> match_parameter_sequence()
    {
        Token first_token;
        std::vector<Node_Handle> parameters;
        while (true) {
            auto p = match_parameter();
            if (!p) {
                return p;
            }
            first_token = get_token(m_program.get_node(*p));
            parameters.push_back(*p);
            if (!expect(Token_Type::comma)) {
                break;
            }
        }
        BIT_MANIPULATION_ASSERT(!parameters.empty());

        return m_program.push_node(Parameter_List_Node { first_token, std::move(parameters) });
    }

    Result<Node_Handle, Rule_Error> match_parameter()
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
        return m_program.push_node(Parameter_Node { *id, id->extract(m_program.source), *type });
    }

    Result<Node_Handle, Rule_Error> match_static_assertion()
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
        return m_program.push_node(Static_Assert_Node { *t, *expression });
    }

    Result<Node_Handle, Rule_Error> match_requires_clause()
    {
        constexpr auto this_rule = Grammar_Rule::requires_clause;
        const Token* t = expect(Token_Type::keyword_requires);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_requires> };
        }
        return match_expression();
    }

    Result<Node_Handle, Rule_Error> match_statement()
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
                                                         Token_Type::identifier };

        if (const Token* t = peek()) {
            switch (t->type) {
            case Token_Type::keyword_let: return match_let_declaration();
            case Token_Type::keyword_const: return match_const_declaration();
            case Token_Type::keyword_static_assert: return match_static_assertion();
            case Token_Type::keyword_break: return match_break_statement();
            case Token_Type::keyword_continue: return match_continue_statement();
            case Token_Type::keyword_return: return match_return_statement();
            case Token_Type::keyword_if: return match_if_statement();
            case Token_Type::keyword_while: return match_while_statement();
            case Token_Type::left_brace: return match_block_statement();
            case Token_Type::identifier: return match_assignment();
            default: break;
            }
        }

        return Rule_Error { this_rule, possible_types };
    }

    Result<Node_Handle, Rule_Error> match_assignment_statement()
    {
        constexpr auto this_rule = Grammar_Rule::assignment_statement;
        auto a = match_assignment();
        if (!a) {
            return a;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return a;
    }

    Result<Node_Handle, Rule_Error> match_assignment()
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
        return m_program.push_node(Assignment_Node { *id, id->extract(m_program.source), *e });
    }

    Result<Node_Handle, Rule_Error> match_return_statement()
    {
        constexpr auto this_rule = Grammar_Rule::return_statement;
        const Token* t = expect(Token_Type::keyword_return);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_return> };
        }
        auto e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return m_program.push_node(Return_Statement_Node { *t, *e });
    }

    Result<Node_Handle, Rule_Error> match_break_statement()
    {
        constexpr auto this_rule = Grammar_Rule::break_statement;
        const Token* t = expect(Token_Type::keyword_break);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_break> };
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return m_program.push_node(Jump_Node { *t });
    }

    Result<Node_Handle, Rule_Error> match_continue_statement()
    {
        constexpr auto this_rule = Grammar_Rule::continue_statement;
        const Token* t = expect(Token_Type::keyword_continue);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_continue> };
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return m_program.push_node(Jump_Node { *t });
    }

    Result<Node_Handle, Rule_Error> match_if_statement()
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
        auto else_result = [this]() -> Result<Node_Handle, Rule_Error> {
            if (!peek(Token_Type::keyword_else)) {
                return Node_Handle::null;
            }
            return match_else_statement();
        }();
        if (!else_result) {
            return else_result;
        }

        return m_program.push_node(If_Statement_Node { *first, *condition, *block, *else_result });
    }

    Result<Node_Handle, Rule_Error> match_else_statement()
    {
        const auto this_rule = Grammar_Rule::else_statement;

        const Token* first = expect(Token_Type::keyword_else);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_else> };
        }
        return peek(Token_Type::keyword_if) ? match_if_statement() : match_block_statement();
    }

    Result<Node_Handle, Rule_Error> match_while_statement()
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
        return m_program.push_node(While_Statement_Node { *first, *condition, *block });
    }

    Result<Node_Handle, Rule_Error> match_block_statement()
    {
        constexpr auto this_rule = Grammar_Rule::block_statement;
        const Token* first = expect(Token_Type::left_brace);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_brace> };
        }
        std::vector<Node_Handle> statements;
        while (true) {
            if (expect(Token_Type::right_brace)) {
                return m_program.push_node(Block_Statement_Node { *first, std::move(statements) });
            }
            else if (auto s = match_statement()) {
                statements.push_back(*s);
            }
            else {
                return s;
            }
        }
        BIT_MANIPULATION_UNREACHABLE();
    }

    Result<Node_Handle, Rule_Error> match_expression()
    {
        return match_if_expression();
    }

    Result<Node_Handle, Rule_Error> match_if_expression()
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
        return m_program.push_node(
            If_Expression_Node { get_token(m_program.get_node(*left)), *left, *condition, *right });
    }

    Result<Node_Handle, Rule_Error> match_binary_expression()
    {
        if (auto comp = expect(&Parser::match_comparison_expression)) {
            return comp;
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
        return m_program.push_node(Binary_Expression_Node { *op, *left, *right, op->type });
    }

    Result<Node_Handle, Rule_Error> match_comparison_expression()
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
        return m_program.push_node(Binary_Expression_Node { *op, *left, *right, op->type });
    }

    Result<Node_Handle, Rule_Error> match_arithmetic_expression()
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
        return m_program.push_node(Binary_Expression_Node { *op, *left, *right, op->type });
    }

    Result<Node_Handle, Rule_Error> match_prefix_expression()
    {
        if (const Token* t = expect(is_unary_operator)) {
            auto e = match_postfix_expression();
            if (!e) {
                return e;
            }
            return m_program.push_node(Prefix_Expression_Node { *t, t->type, *e });
        }
        return match_postfix_expression();
    }

    Result<Node_Handle, Rule_Error> match_postfix_expression()
    {
        if (auto call = expect(&Parser::match_function_call_expression)) {
            return call;
        }
        return match_primary_expression();
    }

    Result<Node_Handle, Rule_Error> match_function_call_expression()
    {
        constexpr auto this_rule = Grammar_Rule::function_call_expression;
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::left_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }

        std::vector<Node_Handle> arguments;
        for (bool demand_expression = false; true;) {
            if (!demand_expression && expect(Token_Type::right_parenthesis)) {
                break;
            }
            auto arg = match_expression();
            if (!arg) {
                return arg;
            }
            arguments.push_back(*arg);
            demand_expression = expect(Token_Type::comma);
        }
        return m_program.push_node(Function_Call_Expression_Node {
            *id, id->extract(m_program.source), std::move(arguments) });
    }

    Result<Node_Handle, Rule_Error> match_primary_expression()
    {
        constexpr auto this_rule = Grammar_Rule::primary_expression;
        static constexpr Token_Type expected[]
            = { Token_Type::binary_literal,  Token_Type::octal_literal,
                Token_Type::decimal_literal, Token_Type::hexadecimal_literal,
                Token_Type::identifier,      Token_Type::left_parenthesis };

        if (const Token* t = expect(is_literal)) {
            return m_program.push_node(Literal_Node { *t });
        }
        if (const Token* t = expect(Token_Type::identifier)) {
            return m_program.push_node(Id_Expression_Node { *t });
        }
        if (auto e = match_parenthesized_expression()) {
            return e;
        }
        // Intentionally forget about the e error to not falsely suggest that parentheses are
        // required for a primary expression, but give feedback about the whole union.
        return Rule_Error { this_rule, expected };
    }

    [[maybe_unused]] Result<Node_Handle, Rule_Error> match_parenthesized_expression()
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

    Result<Node_Handle, Rule_Error> match_type()
    {
        constexpr auto this_rule = Grammar_Rule::type;
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_bool, Token_Type::keyword_int, Token_Type::keyword_uint };

        if (const Token* t = expect(Token_Type::keyword_bool)) {
            return m_program.push_node(Type_Node { *t, Concrete_Type::Bool });
        }
        if (const Token* t = expect(Token_Type::keyword_int)) {
            return m_program.push_node(Type_Node { *t, Concrete_Type::Int });
        }
        if (const Token* t = expect(Token_Type::keyword_uint)) {
            auto e = match_parenthesized_expression();
            if (!e) {
                return e;
            }
            return m_program.push_node(Type_Node { *t, Bit_Generic_Type { Type_Type::Uint, *e } });
        }

        return Rule_Error { this_rule, expected };
    }
};

} // namespace

Result<Parsed_Program, Parse_Error> parse(std::span<const Token> tokens, std::string_view source)
{
    return Parser(tokens, source).parse();
}

} // namespace bit_manipulation::bms
