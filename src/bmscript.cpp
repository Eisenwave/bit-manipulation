#include <optional>
#include <variant>

#include "assert.hpp"
#include "bmscript.hpp"

using namespace bit_manipulation::ast;

namespace bit_manipulation {

[[nodiscard]] std::string_view token_type_name(Token_Type type)
{
    using enum Token_Type;

    switch (type) {
    case identifier: return "identifier";
    case left_parenthesis: return "left_parenthesis";
    case right_parenthesis: return "right_parenthesis";
    case decimal_literal: return "decimal_literal";
    case octal_literal: return "octal_literal";
    case hexadecimal_literal: return "hexadecimal_literal";
    case binary_literal: return "binary_literal";
    case left_brace: return "left_brace";
    case right_brace: return "right_brace";
    case block_comment: return "block_comment";
    case line_comment: return "line_comment";
    case assign: return "assign";
    case equals: return "equals";
    case not_equals: return "not_equals";
    case plus: return "plus";
    case minus: return "minus";
    case multiplication: return "multiplication";
    case division: return "division";
    case remainder: return "remainder";
    case less_than: return "less_than";
    case greater_than: return "greater_than";
    case less_or_equal: return "less_or_equal";
    case greater_or_equal: return "greater_or_equal";
    case shift_left: return "shift_left";
    case shift_right: return "shift_right";
    case bitwise_and: return "bitwise_and";
    case bitwise_or: return "bitwise_or";
    case bitwise_not: return "bitwise_not";
    case bitwise_xor: return "bitwise_xor";
    case logical_and: return "logical_and";
    case logical_or: return "logical_or";
    case logical_not: return "logical_not";
    case right_arrow: return "right_arrow";
    case double_right_arrow: return "double_right_arrow";
    case dot: return "dot";
    case colon: return "colon";
    case comma: return "comma";
    case semicolon: return "semicolon";
    case keyword_let: return "keyword_let";
    case keyword_const: return "keyword_const";
    case keyword_function: return "keyword_function";
    case keyword_for: return "keyword_for";
    case keyword_while: return "keyword_while";
    case keyword_if: return "keyword_if";
    case keyword_else: return "keyword_else";
    case keyword_uint: return "keyword_uint";
    case keyword_int: return "keyword_int";
    case keyword_bool: return "keyword_bool";
    case keyword_requires: return "keyword_requires";
    case keyword_return: return "keyword_return";
    case keyword_break: return "keyword_break";
    case keyword_continue: return "keyword_continue";
    case keyword_true: return "keyword_true";
    case keyword_false: return "keyword_false";
    }
    return "";
}

[[nodiscard]] std::string_view token_type_readable_name(Token_Type type)
{
    using enum Token_Type;

    switch (type) {
    case identifier: return "identifier";
    case left_parenthesis: return "'('";
    case right_parenthesis: return "')'";
    case decimal_literal: return "decimal literal";
    case octal_literal: return "octal literal";
    case hexadecimal_literal: return "hexadecimal literal";
    case binary_literal: return "binary literal";
    case left_brace: return "'{'";
    case right_brace: return "'}'";
    case block_comment: return "'/*'";
    case line_comment: return "'//'";
    case assign: return "'='";
    case equals: return "'=='";
    case not_equals: return "'!='";
    case plus: return "'+'";
    case minus: return "'-'";
    case multiplication: return "'*'";
    case division: return "'/'";
    case remainder: return "'%'";
    case less_than: return "'<'";
    case greater_than: return "'>'";
    case less_or_equal: return "'<='";
    case greater_or_equal: return "'>='";
    case shift_left: return "'<<'";
    case shift_right: return "'>>'";
    case bitwise_and: return "'&'";
    case bitwise_or: return "'|'";
    case bitwise_not: return "'~'";
    case bitwise_xor: return "'^'";
    case logical_and: return "'&&'";
    case logical_or: return "'||'";
    case logical_not: return "'!'";
    case right_arrow: return "'->'";
    case double_right_arrow: return "'=>'";
    case dot: return "'.'";
    case colon: return "':'";
    case comma: return "','";
    case semicolon: return "';'";
    case keyword_let: return "'let'";
    case keyword_const: return "'const'";
    case keyword_function: return "'function'";
    case keyword_for: return "'for'";
    case keyword_while: return "'while'";
    case keyword_if: return "'if'";
    case keyword_else: return "'else'";
    case keyword_uint: return "'Uint'";
    case keyword_int: return "'Int'";
    case keyword_bool: return "'Bool'";
    case keyword_requires: return "'requires'";
    case keyword_return: return "'return'";
    case keyword_break: return "'break'";
    case keyword_continue: return "'continue'";
    case keyword_true: return "'true'";
    case keyword_false: return "'false'";
    }
    return "";
}

[[nodiscard]] Size token_type_length(Token_Type type)
{
    using enum Token_Type;

    switch (type) {
    case identifier:
    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal:
    case block_comment:
    case line_comment: return 0;

    case left_parenthesis:
    case right_parenthesis:
    case left_brace:
    case right_brace:
    case assign:
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case less_than:
    case greater_than:
    case bitwise_and:
    case bitwise_or:
    case bitwise_not:
    case bitwise_xor:
    case dot:
    case colon:
    case comma:
    case semicolon: return 1;

    case equals:
    case not_equals:
    case less_or_equal:
    case greater_or_equal:
    case shift_left:
    case shift_right:
    case logical_and:
    case logical_or:
    case logical_not:
    case right_arrow:
    case double_right_arrow:
    case keyword_if: return 2;

    case keyword_for:
    case keyword_let:
    case keyword_int: return 3;

    case keyword_else:
    case keyword_uint:
    case keyword_bool:
    case keyword_true: return 4;

    case keyword_const:
    case keyword_break:
    case keyword_while:
    case keyword_false: return 5;

    case keyword_return: return 6;

    case keyword_function:
    case keyword_requires:
    case keyword_continue: return 8;
    }
}

[[nodiscard]] bool is_unary_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case plus:
    case minus:
    case logical_not:
    case bitwise_not: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_literal(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal:
    case keyword_true:
    case keyword_false: return true;
    default: return false;
    }
}

[[nodiscard]] bool is_binary_operator(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case equals:
    case not_equals:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal:
    case logical_and:
    case logical_or:
    case shift_left:
    case shift_right:
    case bitwise_and:
    case bitwise_or:
    case bitwise_xor: return true;
    default: return false;
    }
}

[[nodiscard]] std::string_view grammar_rule_name(Grammar_Rule rule)
{
    using enum Grammar_Rule;
    switch (rule) {
    case program: return "program";
    case program_declaration: return "program_declaration";
    case const_declaration: return "const_declaration";
    case let_declaration: return "let_declaration";
    case initializer: return "initializer";
    case function_declaration: return "function_declaration";
    case function_header: return "function_header";
    case requires_clause: return "requires_clause";
    case parameter_sequence: return "parameter_sequence";
    case parameter: return "parameter";
    case statement: return "statement";
    case assignment_statement: return "assignment_statement";
    case assignment: return "assignment";
    case break_statement: return "break_statement";
    case continue_statement: return "continue_statement";
    case return_statement: return "return_statement";
    case if_statement: return "if_statement";
    case while_statement: return "while_statement";
    case for_statement: return "for_statement";
    case init_clause: return "init_clause";
    case block_statement: return "block_statement";
    case expression: return "expression";
    case if_expression: return "if_expression";
    case binary_expression: return "binary_expression";
    case prefix_expression: return "prefix_expression";
    case postfix_expression: return "postfix_expression";
    case function_call_expression: return "function_call_expression";
    case expression_sequence: return "expression_sequence";
    case primary_expression: return "primary_expression";
    case parenthesized_expression: return "parenthesized_expression";
    case integer_literal: return "integer_literal";
    case binary_operator: return "binary_operator";
    case unary_operator: return "unary_operator";
    case type: return "type";
    case uint: return "uint";
    }
    return "";
}

[[nodiscard]] constexpr std::string_view node_type_name(Node_Type t)
{
    using enum Node_Type;

    switch (t) {
    case program: return "program";
    case function: return "function";
    case parameter: return "parameter";
    case type: return "type";
    case variable: return "variable";
    case statement: return "statement";
    case if_statement: return "if_statement";
    case for_statement: return "for_statement";
    case while_statement: return "while_statement";
    case break_statement: return "break_statement";
    case continue_statement: return "continue_statement";
    case return_statement: return "return_statement";
    case assignment: return "assignment";
    case block_statement: return "block_statement";
    case expression: return "expression";
    case if_expression: return "if_expression";
    case binary_expression: return "binary_expression";
    case prefix_expression: return "prefix_expression";
    case id_expression: return "id_expression";
    case primary_expression: return "primary_expression";
    case function_call_expression: return "function_call_expression";
    case literal: return "literal";
    }
    return "";
}

// =================================================================================================

Program_Data::Program_Data(std::vector<Node>&& declarations)
    : declarations(std::move(declarations))
{
}

Function_Data::Function_Data(std::string_view name,
                             std::vector<Node>&& parameters,
                             Node&& requires_clause,
                             Node&& return_type,
                             Node&& body)
    : name(name)
    , parameters(std::move(parameters))
    , requires_clause(std::make_unique<Node>(std::move(requires_clause)))
    , return_type(std::make_unique<Node>(std::move(return_type)))
    , body(std::make_unique<Node>(std::move(body)))
{
}

Function_Data::Function_Data(std::string_view name,
                             std::vector<Node>&& parameters,
                             Node&& return_type,
                             Node&& body)
    : name(name)
    , parameters(std::move(parameters))
    , return_type(std::make_unique<Node>(std::move(return_type)))
    , body(std::make_unique<Node>(std::move(body)))
{
}

Let_Const_Data Let_Const_Data::type_and_initializer(Token_Type let_or_const,
                                                    std::string_view name,
                                                    Node&& type,
                                                    Node&& initializer)
{
    return { let_or_const == Token_Type::keyword_const, name,
             std::make_unique<Node>(std::move(type)),
             std::make_unique<Node>(std::move(initializer)) };
}

Let_Const_Data
Let_Const_Data::initializer_only(Token_Type let_or_const, std::string_view name, Node&& initializer)
{
    return { let_or_const == Token_Type::keyword_const, name, nullptr,
             std::make_unique<Node>(std::move(initializer)) };
}

Let_Const_Data Let_Const_Data::let_type_only(std::string_view name, Node&& type)
{
    return { false, name, std::make_unique<Node>(std::move(type)), nullptr };
}

Let_Const_Data::Let_Const_Data(bool is_const,
                               std::string_view name,
                               std::unique_ptr<Node> type,
                               std::unique_ptr<Node> initializer)
    : name(name)
    , type(std::move(type))
    , initializer(std::move(initializer))
    , is_const(is_const)
{
}

Assignment_Data::Assignment_Data(std::string_view name, Node&& expression)
    : name(name)
    , expression(std::make_unique<Node>(std::move(expression)))
{
}

Parameter_Data::Parameter_Data(std::string_view name, Node&& type)
    : name(name)
    , expression(std::make_unique<Node>(std::move(type)))
{
}

Return_Statement_Data::Return_Statement_Data(Node&& expression)
    : expression(std::make_unique<Node>(std::move(expression)))
{
}

Block_Statement_Data::Block_Statement_Data(std::vector<Node>&& statements)
    : statements(std::move(statements))
{
}

If_While_Statement_Data::If_While_Statement_Data(Node&& condition, Node&& block)
    : condition(std::make_unique<Node>(std::move(condition)))
    , block(std::make_unique<Node>(std::move(block)))
{
}

For_Statement_Data::For_Statement_Data(Node&& init,
                                       Node&& condition,
                                       Node&& increment,
                                       Node&& block)
    : init(std::make_unique<Node>(std::move(init)))
    , condition(std::make_unique<Node>(std::move(condition)))
    , increment(std::make_unique<Node>(std::move(increment)))
    , block(std::make_unique<Node>(std::move(block)))
{
}

For_Statement_Data::For_Statement_Data(Node&& init, Node&& condition, Node&& block)
    : init(std::make_unique<Node>(std::move(init)))
    , condition(std::make_unique<Node>(std::move(condition)))
    , block(std::make_unique<Node>(std::move(block)))
{
}

If_Expression_Data::If_Expression_Data(Node&& left, Node&& condition, Node&& right)
    : condition(std::make_unique<Node>(std::move(condition)))
    , left(std::make_unique<Node>(std::move(left)))
    , right(std::make_unique<Node>(std::move(right)))
{
}

Binary_Expression_Data::Binary_Expression_Data(Node&& left, Node&& right, Token_Type op)
    : left(std::make_unique<Node>(std::move(left)))
    , right(std::make_unique<Node>(std::move(right)))
    , op(op)
{
}

Prefix_Expression_Data::Prefix_Expression_Data(Node&& operand, Token_Type op)
    : operand(std::make_unique<Node>(std::move(operand)))
    , op(op)
{
}

Function_Call_Expression_Data::Function_Call_Expression_Data(std::string_view function,
                                                             std::vector<Node>&& arguments)
    : function(function)
    , arguments(std::move(arguments))
{
}

Type_Data Type_Data::make_bool()
{
    return { Type_Type::Bool, nullptr };
}

Type_Data Type_Data::make_int()
{
    return { Type_Type::Int, nullptr };
}

Type_Data Type_Data::make_uint(Node&& width)
{
    return { Type_Type::Uint, std::make_unique<Node>(std::move(width)) };
}

Type_Data::Type_Data(Type_Type type, std::unique_ptr<Node> width)
    : width(std::move(width))
    , type(type)
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
public:
    using Node_Type = Node;

private:
    std::variant<Node_Type, Rule_Error> m_data;

public:
    Rule_Result(Node_Type&& n)
        : m_data(std::move(n))
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

    Node_Type& operator*()
    {
        BIT_MANIPULATION_ASSERT(has_value());
        return *operator->();
    }

    Node_Type* operator->()
    {
        return std::get_if<Node_Type>(&m_data);
    }

    const Node_Type& get_node() const
    {
        return std::get<Node_Type>(m_data);
    }

    const Rule_Error& get_error() const
    {
        return std::get<Rule_Error>(m_data);
    }
};

struct Parser {
private:
    std::span<const Token> m_tokens;
    std::string_view m_source;
    Size m_pos;

    struct Attempt {
        Parser& parser;
        const Size pos;
        bool commited = false;

        Attempt(Parser& parser, Size pos) noexcept
            : parser { parser }
            , pos { pos }
        {
        }

        Attempt(const Attempt&) = delete;

        ~Attempt()
        {
            if (!commited) {
                parser.m_pos = pos;
            }
        }

        void commit()
        {
            commited = true;
        }
    };

public:
    explicit Parser(std::span<const Token> tokens, std::string_view source)
        : m_tokens { tokens }
        , m_source { source }
        , m_pos { 0 }
    {
    }

    Parse_Result parse()
    {
        if (Rule_Result program = match_program()) {
            return std::move(*program);
        }
        else {
            const auto fail_token = m_pos < m_tokens.size() ? m_tokens[m_pos] : Token {};
            const auto [fail_rule, expected_tokens] = program.get_error();
            return Parse_Error { fail_rule, expected_tokens, fail_token };
        }
    }

private:
    const Token* peek() const
    {
        return m_pos < m_tokens.size() ? &m_tokens[m_pos] : nullptr;
    }

    const Token* peek(Token_Type expected)
    {
        if (const Token* next = peek(); next && next->type == expected) {
            return next;
        }
        return nullptr;
    }

    /// @brief Advances the position of the parser by one.
    /// There must be a token to pop, otherwise this function fails.
    /// This function can only be used safely in conjunction with `peek`.
    void pop()
    {
        BIT_MANIPULATION_ASSERT(m_pos < m_tokens.size());
        m_pos += 1;
    }

    bool eof()
    {
        return m_pos == m_tokens.size();
    }

    /// @brief Checks whether the next token (if any) equals the expected type.
    /// If so, the parser advances by one token.
    /// Otherwise, does nothing and returns `nullptr`.
    /// @param type the expected type
    /// @return The popped token with the given type, or `nullptr` if there is no token, or the
    /// token doesn't match the expected type.
    const Token* expect(Token_Type type)
    {
        if (const Token* next = peek(); next && next->type == type) {
            m_pos += 1;
            return next;
        }
        return nullptr;
    }

    Attempt start_attempt()
    {
        return { *this, m_pos };
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
        case Grammar_Rule::const_declaration: return match_variable(Token_Type::keyword_const);
        case Grammar_Rule::let_declaration: return match_variable(Token_Type::keyword_let);
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
        case Grammar_Rule::if_statement: return match_if_or_while(Token_Type::keyword_if);
        case Grammar_Rule::while_statement: return match_if_or_while(Token_Type::keyword_while);
        case Grammar_Rule::for_statement: return match_for_statement();
        case Grammar_Rule::init_clause: return match_init_clause();
        case Grammar_Rule::block_statement: return match_block_statement();
        case Grammar_Rule::expression: return match_expression();
        case Grammar_Rule::if_expression: return match_if_expression();
        case Grammar_Rule::binary_expression: return match_binary_expression();
        case Grammar_Rule::prefix_expression: return match_prefix_expression();
        case Grammar_Rule::postfix_expression: return match_postfix_expression();
        case Grammar_Rule::function_call_expression: return match_function_call_expression();
        case Grammar_Rule::primary_expression: return match_primary_expression();
        case Grammar_Rule::parenthesized_expression: return match_parenthesized_expression();
        case Grammar_Rule::type: return match_type();
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    /// @brief Like `match`, but the parser state is not advanced if no match was made.
    /// @param rule the grammar rule
    /// @return the matched result, or `rule`
    Rule_Result expect(Grammar_Rule rule)
    {
        {
            auto attempt = start_attempt();
            if (Rule_Result result = match(rule)) {
                attempt.commit();
                return result;
            }
        }
        return Rule_Error { rule };
    }

    Rule_Result match_program()
    {
        Rule_Result first = match_program_declaration();
        if (!first) {
            return first;
        }
        std::vector<Node> declarations;
        declarations.push_back(std::move(*first));

        while (!eof()) {
            Rule_Result d = match_program_declaration();
            if (!d) {
                return d;
            }
            declarations.push_back(std::move(*d));
        }
        return Node { first->token, Node_Type::program, Program_Data { std::move(declarations) } };
    }

    Rule_Result match_program_declaration()
    {
        if (peek(Token_Type::keyword_const)) {
            return match_variable(Token_Type::keyword_const);
        }
        if (peek(Token_Type::keyword_function)) {
            return match_function_declaration();
        }
        return Rule_Error { Grammar_Rule::program_declaration };
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
        const std::string_view name = id->extract(m_source);

        if (expect(Token_Type::colon)) {
            Rule_Result type = match_type();
            if (!type) {
                return type;
            }
            if (Rule_Result init = expect(Grammar_Rule::initializer)) {
                if (!expect(Token_Type::semicolon)) {
                    return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
                }
                return Node { *t, Node_Type::variable,
                              Let_Const_Data::type_and_initializer(
                                  const_or_let, name, std::move(*type), std::move(*init)) };
            }
            if (const_or_let == Token_Type::keyword_let && expect(Token_Type::semicolon)) {
                return Node { *t, Node_Type::variable,
                              Let_Const_Data::let_type_only(name, std::move(*type)) };
            }
        }
        Rule_Result init = match_initializer();
        if (!init) {
            return init;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::identifier> };
        }
        return Node { *t, Node_Type::variable,
                      Let_Const_Data::initializer_only(const_or_let, name, std::move(*init)) };
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

        std::vector<Node> parameters;
        if (Rule_Result p0 = expect(Grammar_Rule::parameter)) {
            parameters.push_back(std::move(*p0));
            while (expect(Token_Type::comma)) {
                if (Rule_Result p = match_parameter()) {
                    parameters.push_back(std::move(*p));
                }
                else {
                    return p;
                }
            }
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
        if (peek(Token_Type::keyword_requires)) {
            Rule_Result req = expect(Grammar_Rule::requires_clause);
            if (!req) {
                return req;
            }
            Rule_Result body = match_block_statement();
            if (!body) {
                return body;
            }
            return Node { *t, Node_Type::function,
                          Function_Data { name->extract(m_source), std::move(parameters),
                                          std::move(*req), std::move(*ret), std::move(*body) } };
        }
        Rule_Result body = match_block_statement();
        if (!body) {
            return body;
        }
        return Node { *t, Node_Type::function,
                      Function_Data { name->extract(m_source), std::move(parameters),
                                      std::move(*ret), std::move(*body) } };
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
        return Node { *id, Node_Type::parameter,
                      Parameter_Data { id->extract(m_source), std::move(*type) } };
    }

    Rule_Result match_requires_clause()
    {
        constexpr auto this_rule = Grammar_Rule::requires_clause;
        // TODO
        return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_requires> };
    }

    Rule_Result match_statement()
    {
        constexpr auto this_rule = Grammar_Rule::statement;
        // This is a manually computed FIRST set of the statement rule.
        static constexpr Token_Type possible_types[]
            = { Token_Type::keyword_let,      Token_Type::keyword_const,  Token_Type::keyword_break,
                Token_Type::keyword_continue, Token_Type::keyword_return, Token_Type::keyword_if,
                Token_Type::keyword_while,    Token_Type::keyword_for,    Token_Type::left_brace,
                Token_Type::identifier };

        if (const Token* t = peek()) {
            switch (t->type) {
            case Token_Type::keyword_let: return match_variable(Token_Type::keyword_let);
            case Token_Type::keyword_const: return match_variable(Token_Type::keyword_const);
            case Token_Type::keyword_break: return match_break_statement();
            case Token_Type::keyword_continue: return match_continue_statement();
            case Token_Type::keyword_return: return match_return_statement();
            case Token_Type::keyword_if: return match_if_or_while(Token_Type::keyword_if);
            case Token_Type::keyword_while: return match_if_or_while(Token_Type::keyword_while);
            case Token_Type::keyword_for: return match_for_statement();
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
        return Node { *id, Node_Type::assignment,
                      Assignment_Data { id->extract(m_source), std::move(*e) } };
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
        return Node { *t, Node_Type::return_statement, Return_Statement_Data { std::move(*e) } };
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
        return Node { *t, Node_Type::break_statement };
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
        return Node { *t, Node_Type::continue_statement };
    }

    Rule_Result match_if_or_while(Token_Type if_or_while)
    {
        BIT_MANIPULATION_ASSERT(if_or_while == Token_Type::keyword_if
                                || if_or_while == Token_Type::keyword_while);

        const auto this_rule = if_or_while == Token_Type::keyword_if
            ? Grammar_Rule::if_statement
            : Grammar_Rule::while_statement;
        const std::span<const Token_Type> expected_tokens = if_or_while == Token_Type::keyword_if
            ? const_array_one_v<Token_Type::keyword_if>
            : const_array_one_v<Token_Type::keyword_while>;

        const Token* first = expect(if_or_while);
        if (!first) {
            return Rule_Error { this_rule, expected_tokens };
        }
        if (!expect(Token_Type::left_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }
        Rule_Result condition = match_expression();
        if (!condition) {
            return condition;
        }
        if (!expect(Token_Type::right_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
        }
        Rule_Result block = match_block_statement();
        if (!block) {
            return block;
        }
        const auto type = if_or_while == Token_Type::keyword_if ? Node_Type::if_statement
                                                                : Node_Type::while_statement;
        return Node { *first, type,
                      If_While_Statement_Data { std::move(*condition), std::move(*block) } };
    }

    Rule_Result match_for_statement()
    {
        constexpr auto this_rule = Grammar_Rule::for_statement;
        const Token* first = expect(Token_Type::keyword_for);
        if (!first) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::keyword_for> };
        }
        if (expect(Token_Type::left_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
        }
        Rule_Result init = match_init_clause();
        if (!init) {
            return init;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        Rule_Result condition = match_expression();
        if (!condition) {
            return condition;
        }
        if (!expect(Token_Type::semicolon)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::semicolon> };
        }
        if (Rule_Result increment = expect(Grammar_Rule::assignment)) {
            if (!expect(Token_Type::right_parenthesis)) {
                return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
            }
            Rule_Result block = match_block_statement();
            if (!block) {
                return block;
            }
            return Node { *first, Node_Type::for_statement,
                          For_Statement_Data { std::move(*init), std::move(*condition),
                                               std::move(*increment), std::move(*block) } };
        }
        if (!expect(Token_Type::right_parenthesis)) {
            return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
        }
        Rule_Result block = match_block_statement();
        if (!block) {
            return block;
        }
        return Node { *first, Node_Type::for_statement,
                      For_Statement_Data { std::move(*init), std::move(*condition),
                                           std::move(*block) } };
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
        std::vector<Node> statements;
        while (true) {
            if (expect(Token_Type::right_brace)) {
                return Node { *first, Node_Type::block_statement,
                              Block_Statement_Data { std::move(statements) } };
            }
            else if (Rule_Result s = match_statement()) {
                statements.push_back(std::move(*s));
            }
            else {
                return s;
            }
        }
        // unreachable
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
        return Node { left->token, Node_Type::binary_expression,
                      If_Expression_Data { std::move(*left), std::move(*condition),
                                           std::move(*right) } };
    }

    Rule_Result match_binary_expression()
    {
        Rule_Result left = match_prefix_expression();
        if (!left) {
            return left;
        }
        { // TODO: use function pointers to get rid of these manual attempts
            auto attempt = start_attempt();
            if (const Token* op = peek(); op && is_binary_operator(op->type)) {
                attempt.commit();
                pop();
                Rule_Result right = match_prefix_expression();
                if (!right) {
                    return right;
                }
                return Node { left->token, Node_Type::binary_expression,
                              Binary_Expression_Data { std::move(*left), std::move(*right),
                                                       op->type } };
            }
        }
        return left;
    }

    Rule_Result match_prefix_expression()
    {
        {
            auto attempt = start_attempt();
            if (const Token* t = peek(); t && is_unary_operator(t->type)) {
                attempt.commit();
                pop();
                Rule_Result e = match_prefix_expression();
                if (!e) {
                    return e;
                }
                return Node { *t, Node_Type::prefix_expression,
                              Prefix_Expression_Data { std::move(*e), t->type } };
            }
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
        if (expect(Token_Type::right_parenthesis)) {
            return Node { *id, Node_Type::function_call_expression,
                          Function_Call_Expression_Data { id->extract(m_source), {} } };
        }

        std::vector<Node> arguments;
        while (true) {
            // Matching instead of expecting is correct here because the possibility of an
            // empty parameter list has already been eliminated.
            Rule_Result arg = match_expression();
            if (!arg) {
                return arg;
            }
            arguments.push_back(std::move(*arg));
            if (expect(Token_Type::comma)) {
                continue;
            }
            if (expect(Token_Type::right_parenthesis)) {
                break;
            }
        }
        return Node { *id, Node_Type::function_call_expression,
                      Function_Call_Expression_Data { id->extract(m_source),
                                                      std::move(arguments) } };
    }

    Rule_Result match_primary_expression()
    {
        constexpr auto this_rule = Grammar_Rule::primary_expression;
        static constexpr Token_Type expected[]
            = { Token_Type::binary_literal,  Token_Type::octal_literal,
                Token_Type::decimal_literal, Token_Type::hexadecimal_literal,
                Token_Type::identifier,      Token_Type::left_parenthesis };

        const Token* t = peek();
        if (!t) {
            return Rule_Error { this_rule, expected };
        }
        if (is_literal(t->type)) {
            pop();
            return Node { *t, Node_Type::literal };
        }
        if (t->type == Token_Type::identifier) {
            pop();
            return Node { *t, Node_Type::id_expression };
        }
        if (t->type == Token_Type::left_parenthesis) {
            pop();
            Rule_Result e = match_expression();
            if (!e) {
                return e;
            }
            if (expect(Token_Type::right_parenthesis)) {
                return e;
            }
        }
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
        const Token* t = peek();
        if (!t) {
            return Rule_Error { this_rule, expected };
        }
        switch (t->type) {
        case Token_Type::keyword_bool:
            pop();
            return Node { *t, Node_Type::type, Type_Data::make_bool() };

        case Token_Type::keyword_int:
            pop();
            return Node { *t, Node_Type::type, Type_Data::make_int() };

        case Token_Type::keyword_uint: {
            pop();
            if (!expect(Token_Type::left_parenthesis)) {
                return Rule_Error { this_rule, const_array_one_v<Token_Type::left_parenthesis> };
            }
            Rule_Result e = match_expression();
            if (!e) {
                return e;
            }
            if (!expect(Token_Type::right_parenthesis)) {
                return Rule_Error { this_rule, const_array_one_v<Token_Type::right_parenthesis> };
            }
            return Node { *t, Node_Type::type, Type_Data::make_uint(std::move(*e)) };
        }

        default: return Rule_Error { this_rule, expected };
        }
    }
};

} // namespace

Parse_Result parse(std::span<const Token> tokens, std::string_view source)
{
    return Parser(tokens, source).parse();
}

} // namespace bit_manipulation
