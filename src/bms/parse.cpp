#include <optional>
#include <variant>

#include "assert.hpp"

#include "bms/parsing.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

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
    , Parent<4> { parameters, requires_clause, return_type, body }
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

Let_Const_Node::Let_Const_Node(Token token,
                               Token_Type let_or_const,
                               std::string_view name,
                               Node_Handle type,
                               Node_Handle initializer)
    : Node_Base { token }
    , Parent<2> { type, initializer }
    , name(name)
    , is_const(let_or_const == Token_Type::keyword_const)
{
    BIT_MANIPULATION_ASSERT(type != Node_Handle::null || initializer != Node_Handle::null);
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

struct Rule_Error {
    Grammar_Rule rule;
    std::span<const Token_Type> expected_tokens = {};
};

struct Rule_Result {
private:
    std::variant<Node_Handle, Rule_Error> m_data;

public:
    Rule_Result(Node_Handle n)
        : m_data((n))
    {
    }

    Rule_Result(Rule_Error fail)
        : m_data(fail)
    {
    }

    [[nodiscard]] bool has_value() const noexcept
    {
        return m_data.index() == 0;
    }

    [[nodiscard]] operator bool() const noexcept
    {
        return has_value();
    }

    Node_Handle& operator*()
    {
        BIT_MANIPULATION_ASSERT(has_value());
        return std::get<Node_Handle>(m_data);
    }

    const Node_Handle& operator*() const
    {
        BIT_MANIPULATION_ASSERT(has_value());
        return std::get<Node_Handle>(m_data);
    }

    Node_Handle get_node() const
    {
        return std::get<Node_Handle>(m_data);
    }

    Rule_Error get_error() const
    {
        return std::get<Rule_Error>(m_data);
    }
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

    Parse_Result parse()
    {
        if (Rule_Result program = match_program()) {
            return std::move(m_program);
        }
        else {
            const auto fail_token = m_pos < m_tokens.size() ? m_tokens[m_pos] : Token {};
            const auto [fail_rule, expected_tokens] = program.get_error();
            return Parse_Error { fail_rule, expected_tokens, fail_token };
        }
    }

private:
    bool eof()
    {
        skip_comments();
        return m_pos == m_tokens.size();
    }

    void skip_comments()
    {
        while (m_pos != m_tokens.size() && is_comment(m_tokens[m_pos].type)) {
            m_pos += 1;
        }
    }

    template <std::invocable<Token_Type> Predicate>
    const Token* peek_or_expect(Predicate p, bool increment)
    {
        if (const Token* next = peek(); next && p(next->type)) {
            m_pos += increment;
            return next;
        }
        return nullptr;
    }

    const Token* peek()
    {
        return !eof() ? &m_tokens[m_pos] : nullptr;
    }

    const Token* peek(Token_Type expected)
    {
        return peek_or_expect([=](Token_Type t) { return t == expected; }, false);
    }

    const Token* peek(bool (&predicate)(Token_Type))
    {
        return peek_or_expect(predicate, false);
    }

    /// @brief Checks whether the next token (if any) equals the expected type.
    /// If so, the parser advances by one token.
    /// Otherwise, does nothing and returns `nullptr`.
    /// @param type the expected type
    /// @return The popped token with the given type, or `nullptr` if there is no token, or the
    /// token doesn't match the expected type.
    const Token* expect(Token_Type type)
    {
        return peek_or_expect([=](Token_Type t) { return t == type; }, true);
    }

    const Token* expect(bool (&predicate)(Token_Type))
    {
        return peek_or_expect(predicate, true);
    }

    /// @brief Like `match`, but the parser state is not advanced if no match was made.
    /// @param rule the grammar rule
    /// @return the matched result, or `rule`
    Rule_Result expect(Grammar_Rule rule)
    {
        const Size restore_pos = m_pos;
        Rule_Result result = match(rule);
        if (!result) {
            m_pos = restore_pos;
        }
        return result;
    }

    /// @brief Matches a grammatical rule.
    /// Not every rule is supported; only those which have an equivalent AST node.
    /// The parser state is advanced up to the token where the rule failed to match.
    /// @param rule the grammar rule
    /// @return the matched result, or `rule`
    Rule_Result match(Grammar_Rule rule)
    {
        switch (rule) {
        case Grammar_Rule::program: return match_program();
        case Grammar_Rule::program_declaration: return match_program_declaration();
        case Grammar_Rule::const_declaration: return match_const_declaration();
        case Grammar_Rule::let_declaration: return match_let_declaration();
        case Grammar_Rule::initializer: return match_initializer();
        case Grammar_Rule::function_declaration: return match_function_declaration();
        case Grammar_Rule::requires_clause: return match_requires_clause();
        case Grammar_Rule::parameter: return match_parameter();
        case Grammar_Rule::statement: return match_statement();
        case Grammar_Rule::assignment_statement: return match_assignment_statement();
        case Grammar_Rule::assignment: return match_assignment();
        case Grammar_Rule::break_statement: return match_break_statement();
        case Grammar_Rule::continue_statement: return match_continue_statement();
        case Grammar_Rule::return_statement: return match_return_statement();
        case Grammar_Rule::if_statement: return match_if_statement();
        case Grammar_Rule::while_statement: return match_while_statement();
        case Grammar_Rule::init_clause: return match_init_clause();
        case Grammar_Rule::block_statement: return match_block_statement();
        case Grammar_Rule::expression: return match_expression();
        case Grammar_Rule::if_expression: return match_if_expression();
        case Grammar_Rule::binary_expression: return match_binary_expression();
        case Grammar_Rule::comparison_expression: return match_comparison_expression();
        case Grammar_Rule::arithmetic_expression: return match_arithmetic_expression();
        case Grammar_Rule::prefix_expression: return match_prefix_expression();
        case Grammar_Rule::postfix_expression: return match_postfix_expression();
        case Grammar_Rule::function_call_expression: return match_function_call_expression();
        case Grammar_Rule::primary_expression: return match_primary_expression();
        case Grammar_Rule::parenthesized_expression: return match_parenthesized_expression();
        case Grammar_Rule::type: return match_type();
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    Rule_Result match_program()
    {
        Rule_Result first = match_program_declaration();
        if (!first) {
            return first;
        }
        std::vector<Node_Handle> declarations;
        declarations.push_back(*first);

        while (!eof()) {
            Rule_Result d = match_program_declaration();
            if (!d) {
                return d;
            }
            declarations.push_back(*d);
        }
        return m_program.push_node(
            Program_Node { get_token(m_program.get_node(*first)), std::move(declarations) });
    }

    Rule_Result match_program_declaration()
    {
        if (peek(Token_Type::keyword_const)) {
            return match_const_declaration();
        }
        if (peek(Token_Type::keyword_function)) {
            return match_function_declaration();
        }
        return Rule_Error { Grammar_Rule::program_declaration };
    }

    Rule_Result match_let_declaration()
    {
        return match_variable(Token_Type::keyword_let);
    }

    Rule_Result match_const_declaration()
    {
        return match_variable(Token_Type::keyword_const);
    }

    Rule_Result match_variable(Token_Type const_or_let)
    {
        BIT_MANIPULATION_ASSERT(const_or_let == Token_Type::keyword_const
                                || const_or_let == Token_Type::keyword_let);

        const auto this_rule = const_or_let == Token_Type::keyword_let
            ? Grammar_Rule::let_declaration
            : Grammar_Rule::const_declaration;
        const std::span<const Token_Type> expected_tokens = const_or_let == Token_Type::keyword_let
            ? const_array_one_v<Token_Type::keyword_let>
            : const_array_one_v<Token_Type::keyword_const>;

        const Token* t = expect(const_or_let);
        if (!t) {
            return Rule_Error { this_rule, expected_tokens };
        }
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        const std::string_view name = id->extract(m_program.source);

        auto type_handle = Node_Handle::null;
        if (expect(Token_Type::colon)) {
            if (Rule_Result type = match_type()) {
                type_handle = *type;
            }
            else {
                return type;
            }
        }
        const bool mandatory_initializer
            = const_or_let == Token_Type::keyword_const || type_handle == Node_Handle::null;
        Rule_Result init = match_initializer();
        if (!init && mandatory_initializer) {
            return init;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        return m_program.push_node(Let_Const_Node { *t, const_or_let, name, type_handle, *init });
    }

    Rule_Result match_initializer()
    {
        constexpr auto this_rule = Grammar_Rule::initializer;
        if (!expect(Token_Type::assign)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
        }
        Rule_Result e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return e;
    }

    Rule_Result match_function_declaration()
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
            Rule_Result r = match_parameter_sequence();
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
        Rule_Result ret = match_type();
        if (!ret) {
            return ret;
        }
        auto requires_handle = Node_Handle::null;
        if (peek(Token_Type::keyword_requires)) {
            if (Rule_Result req = expect(Grammar_Rule::requires_clause)) {
                requires_handle = *req;
            }
            else {
                return req;
            }
        }
        Rule_Result body = match_block_statement();
        if (!body) {
            return body;
        }
        return m_program.push_node(Function_Node { *t, name->extract(m_program.source),
                                                   std::move(parameters), *ret, requires_handle,
                                                   *body });
    }

    Rule_Result match_parameter_sequence()
    {
        Token first_token;
        std::vector<Node_Handle> parameters;
        while (true) {
            Rule_Result p = match_parameter();
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
        Rule_Result type = match_type();
        if (!type) {
            return type;
        }
        return m_program.push_node(Parameter_Node { *id, id->extract(m_program.source), *type });
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

    Rule_Result match_statement()
    {
        constexpr auto this_rule = Grammar_Rule::statement;
        // This is a manually computed FIRST set of the statement rule.
        static constexpr Token_Type possible_types[]
            = { Token_Type::keyword_let,      Token_Type::keyword_const,  Token_Type::keyword_break,
                Token_Type::keyword_continue, Token_Type::keyword_return, Token_Type::keyword_if,
                Token_Type::keyword_while,    Token_Type::left_brace,     Token_Type::identifier };

        if (const Token* t = peek()) {
            switch (t->type) {
            case Token_Type::keyword_let: return match_let_declaration();
            case Token_Type::keyword_const: return match_const_declaration();
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

    Rule_Result match_assignment_statement()
    {
        constexpr auto this_rule = Grammar_Rule::assignment_statement;
        Rule_Result a = match_assignment();
        if (!a) {
            return a;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return a;
    }

    Rule_Result match_assignment()
    {
        constexpr auto this_rule = Grammar_Rule::assignment;
        const Token* id = expect(Token_Type::identifier);
        if (!id) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        if (!expect(Token_Type::assign)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::assign> };
        }
        Rule_Result e = match_expression();
        if (!e) {
            return e;
        }
        return m_program.push_node(Assignment_Node { *id, id->extract(m_program.source), *e });
    }

    Rule_Result match_return_statement()
    {
        constexpr auto this_rule = Grammar_Rule::return_statement;
        const Token* t = expect(Token_Type::keyword_return);
        if (!t) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_return> };
        }
        Rule_Result e = match_expression();
        if (!e) {
            return e;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        return m_program.push_node(Return_Statement_Node { *t, *e });
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
        return m_program.push_node(Jump_Node { *t });
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
        return m_program.push_node(Jump_Node { *t });
    }

    Rule_Result match_if_statement()
    {
        const auto this_rule = Grammar_Rule::if_statement;

        const Token* first = expect(Token_Type::keyword_if);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_if> };
        }
        Rule_Result condition = match_expression();
        if (!condition) {
            return condition;
        }
        Rule_Result block = match_block_statement();
        if (!block) {
            return block;
        }
        auto else_handle = Node_Handle::null;
        if (expect(Token_Type::keyword_else)) {
            if (Rule_Result else_block = match_block_statement()) {
                else_handle = *else_block;
            }
            else {
                return else_block;
            }
        }
        return m_program.push_node(If_Statement_Node { *first, *condition, *block, else_handle });
    }

    Rule_Result match_while_statement()
    {
        const auto this_rule = Grammar_Rule::while_statement;

        const Token* first = expect(Token_Type::keyword_while);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_while> };
        }
        Rule_Result condition = match_expression();
        if (!condition) {
            return condition;
        }
        Rule_Result block = match_block_statement();
        if (!block) {
            return block;
        }
        return m_program.push_node(While_Statement_Node { *first, *condition, *block });
    }

    Rule_Result match_init_clause()
    {
        constexpr auto this_rule = Grammar_Rule::init_clause;
        static constexpr Token_Type expected[]
            = { Token_Type::keyword_let, Token_Type::identifier };

        if (Rule_Result let = expect(Grammar_Rule::let_declaration)) {
            return let;
        }
        else if (Rule_Result assignment = match_assignment_statement()) {
            return assignment;
        }
        return Rule_Error { this_rule, expected };
    }

    Rule_Result match_block_statement()
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
            else if (Rule_Result s = match_statement()) {
                statements.push_back(*s);
            }
            else {
                return s;
            }
        }
        BIT_MANIPULATION_UNREACHABLE();
    }

    Rule_Result match_expression()
    {
        return match_if_expression();
    }

    Rule_Result match_if_expression()
    {
        constexpr auto this_rule = Grammar_Rule::if_expression;
        Rule_Result left = match_binary_expression();
        if (!left || !expect(Token_Type::keyword_if)) {
            return left;
        }
        Rule_Result condition = match_binary_expression();
        if (!condition) {
            return condition;
        }
        if (!expect(Token_Type::keyword_else)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_else> };
        }
        Rule_Result right = match_binary_expression();
        if (!right) {
            return right;
        }
        return m_program.push_node(
            If_Expression_Node { get_token(m_program.get_node(*left)), *left, *condition, *right });
    }

    Rule_Result match_binary_expression()
    {
        if (Rule_Result comp = expect(Grammar_Rule::comparison_expression)) {
            return comp;
        }

        Rule_Result left = match_prefix_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_binary_operator);
        if (!op) {
            return left;
        }
        Rule_Result right = match_prefix_expression();
        if (!right) {
            return right;
        }
        return m_program.push_node(Binary_Expression_Node { get_token(m_program.get_node(*left)),
                                                            *left, *right, op->type });
    }

    Rule_Result match_comparison_expression()
    {
        constexpr auto this_rule = Grammar_Rule::comparison_expression;
        static constexpr Token_Type expected[]
            = { Token_Type::equals,       Token_Type::not_equals,    Token_Type::less_than,
                Token_Type::greater_than, Token_Type::less_or_equal, Token_Type::greater_or_equal };

        Rule_Result left = match_arithmetic_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_comparison_operator);
        if (!op) {
            return Rule_Error { this_rule, expected };
        }
        Rule_Result right = match_arithmetic_expression();
        if (!right) {
            return right;
        }
        return m_program.push_node(Binary_Expression_Node { get_token(m_program.get_node(*left)),
                                                            *left, *right, op->type });
    }

    Rule_Result match_arithmetic_expression()
    {
        Rule_Result left = match_prefix_expression();
        if (!left) {
            return left;
        }
        const Token* op = expect(is_arithmetic_operator);
        if (!op) {
            return left;
        }
        Rule_Result right = match_prefix_expression();
        if (!right) {
            return right;
        }
        return m_program.push_node(Binary_Expression_Node { get_token(m_program.get_node(*left)),
                                                            *left, *right, op->type });
    }

    Rule_Result match_prefix_expression()
    {
        if (const Token* t = expect(is_unary_operator)) {
            Rule_Result e = match_postfix_expression();
            if (!e) {
                return e;
            }
            return m_program.push_node(Prefix_Expression_Node { *t, t->type, *e });
        }
        return match_postfix_expression();
    }

    Rule_Result match_postfix_expression()
    {
        if (Rule_Result call = expect(Grammar_Rule::function_call_expression)) {
            return call;
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

        std::vector<Node_Handle> arguments;
        for (bool demand_expression = false; true;) {
            if (!demand_expression && expect(Token_Type::right_parenthesis)) {
                break;
            }
            Rule_Result arg = match_expression();
            if (!arg) {
                return arg;
            }
            arguments.push_back(*arg);
            demand_expression = expect(Token_Type::comma);
        }
        return m_program.push_node(Function_Call_Expression_Node {
            *id, id->extract(m_program.source), std::move(arguments) });
    }

    Rule_Result match_primary_expression()
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
        if (Rule_Result e = match_parenthesized_expression()) {
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
        Rule_Result e = match_expression();
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
            = { Token_Type::keyword_bool, Token_Type::keyword_int, Token_Type::keyword_uint };

        if (const Token* t = expect(Token_Type::keyword_bool)) {
            return m_program.push_node(Type_Node { *t, Concrete_Type { Type_Type::Bool } });
        }
        if (const Token* t = expect(Token_Type::keyword_int)) {
            return m_program.push_node(Type_Node { *t, Concrete_Type { Type_Type::Int } });
        }
        if (const Token* t = expect(Token_Type::keyword_uint)) {
            Rule_Result e = match_parenthesized_expression();
            if (!e) {
                return e;
            }
            return m_program.push_node(Type_Node { *t, Bit_Generic_Type { Type_Type::Uint, *e } });
        }

        return Rule_Error { this_rule, expected };
    }
};

} // namespace

Parse_Result parse(std::span<const Token> tokens, std::string_view source)
{
    return Parser(tokens, source).parse();
}

} // namespace bit_manipulation::bms
