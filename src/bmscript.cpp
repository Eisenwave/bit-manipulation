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

struct Rule_Result {
public:
    using Node_Type = Node;

private:
    std::variant<Node_Type, Grammar_Rule> m_data;

public:
    Rule_Result(Node_Type&& n)
        : m_data(std::move(n))
    {
    }

    Rule_Result(Grammar_Rule fail)
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

    const Grammar_Rule& get_error() const
    {
        return std::get<Grammar_Rule>(m_data);
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
            return Parse_Error { program.get_error(), fail_token };
        }
    }

private:
    const Token* peek() const
    {
        return m_pos < m_tokens.size() ? &m_tokens[m_pos] : nullptr;
    }

    /// @brief Advances the position of the parser by one.
    /// There must be a token to pop, otherwise this function fails.
    /// This function can only be used safely in conjunction with `peek`.
    void pop()
    {
        BIT_MANIPULATION_ASSERT(m_pos < m_tokens.size());
        m_pos += 1;
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
        return rule;
    }

    Rule_Result match_union(std::span<const Grammar_Rule> alternatives, Grammar_Rule error)
    {
        for (const auto rule : alternatives) {
            if (Rule_Result r = expect(rule)) {
                return r;
            }
        }
        return error;
    }

    Rule_Result match_program()
    {
        if (Rule_Result first = match_program_declaration()) {
            std::vector<Node> declarations;
            declarations.push_back(std::move(*first));

            while (Rule_Result d = match_program_declaration()) {
                declarations.push_back(std::move(*d));
            }
            if (m_pos == m_tokens.size()) {
                return Node { first->token, Node_Type::program,
                              Program_Data { std::move(declarations) } };
            }
        }

        return Grammar_Rule::program;
    }

    Rule_Result match_program_declaration()
    {
        static constexpr Grammar_Rule rules[] { Grammar_Rule::const_declaration,
                                                Grammar_Rule::function_declaration };
        return match_union(rules, Grammar_Rule::program_declaration);
    }

    Rule_Result match_variable(Token_Type const_or_let)
    {
        BIT_MANIPULATION_ASSERT(const_or_let == Token_Type::keyword_const
                                || const_or_let == Token_Type::keyword_let);

        if (const Token* t = expect(const_or_let)) {
            if (const Token* id = expect(Token_Type::identifier)) {
                const std::string_view name = id->extract(m_source);

                if (expect(Token_Type::colon)) {
                    if (Rule_Result type = match_type()) {
                        {
                            auto attempt = start_attempt();
                            if (Rule_Result init = match_initializer()) {
                                if (expect(Token_Type::semicolon)) {
                                    attempt.commit();
                                    return Node { *t, Node_Type::variable,
                                                  Let_Const_Data::type_and_initializer(
                                                      const_or_let, name, std::move(*type),
                                                      std::move(*init)) };
                                }
                            }
                        }
                        if (expect(Token_Type::semicolon)
                            && const_or_let == Token_Type::keyword_let) {
                            return Node { *t, Node_Type::variable,
                                          Let_Const_Data::let_type_only(name, std::move(*type)) };
                        }
                    }
                    else {
                        return Grammar_Rule::type;
                    }
                }
                else if (Rule_Result init = match_initializer()) {
                    if (expect(Token_Type::semicolon)) {
                        return Node { *t, Node_Type::variable,
                                      Let_Const_Data::initializer_only(const_or_let, name,
                                                                       std::move(*init)) };
                    }
                }
            }
        }

        return const_or_let == Token_Type::keyword_let ? Grammar_Rule::let_declaration
                                                       : Grammar_Rule::const_declaration;
    }

    Rule_Result match_initializer()
    {
        if (expect(Token_Type::assign)) {
            if (Rule_Result e = match_expression()) {
                if (expect(Token_Type::semicolon)) {
                    return e;
                }
            }
        }
        return Grammar_Rule::initializer;
    }

    Rule_Result match_function_declaration()
    {
        if (const Token* t = expect(Token_Type::keyword_function)) {
            if (const Token* name = expect(Token_Type::identifier)) {
                if (expect(Token_Type::left_parenthesis)) {
                    std::vector<Node> parameters;

                    if (Rule_Result p0 = expect(Grammar_Rule::parameter)) {
                        parameters.push_back(std::move(*p0));
                        while (expect(Token_Type::comma)) {
                            if (Rule_Result p = match_parameter()) {
                                parameters.push_back(std::move(*p));
                            }
                            else {
                                return Grammar_Rule::parameter;
                            }
                        }
                    }

                    if (expect(Token_Type::right_parenthesis) && expect(Token_Type::right_arrow)) {
                        if (Rule_Result ret = match_type()) {
                            if (Rule_Result req = expect(Grammar_Rule::requires_clause)) {
                                if (Rule_Result body = match_block_statement()) {
                                    return Node { *t, Node_Type::function,
                                                  Function_Data { name->extract(m_source),
                                                                  std::move(parameters),
                                                                  std::move(*req), std::move(*ret),
                                                                  std::move(*body) } };
                                }
                            }
                            else if (Rule_Result body = match_block_statement()) {
                                return Node { *t, Node_Type::function,
                                              Function_Data { name->extract(m_source),
                                                              std::move(parameters),
                                                              std::move(*ret), std::move(*body) } };
                            }
                        }
                    }
                }
            }
        }
        return Grammar_Rule::function_declaration;
    }

    Rule_Result match_parameter()
    {
        if (const Token* id = expect(Token_Type::identifier)) {
            if (expect(Token_Type::colon)) {
                if (Rule_Result type = match_type()) {
                    return Node { *id, Node_Type::parameter,
                                  Parameter_Data { id->extract(m_source), std::move(*type) } };
                }
            }
        }
        return Grammar_Rule::parameter;
    }

    Rule_Result match_requires_clause()
    {

        return Grammar_Rule::requires_clause;
    }

    Rule_Result match_statement()
    {
        static constexpr Grammar_Rule rules[] {
            Grammar_Rule::let_declaration,      Grammar_Rule::const_declaration,
            Grammar_Rule::assignment_statement, Grammar_Rule::break_statement,
            Grammar_Rule::continue_statement,   Grammar_Rule::return_statement,
            Grammar_Rule::if_statement,         Grammar_Rule::while_statement,
            Grammar_Rule::for_statement,        Grammar_Rule::block_statement
        };
        return match_union(rules, Grammar_Rule::statement);
    }

    Rule_Result match_assignment_statement()
    {
        if (Rule_Result a = match_assignment()) {
            if (expect(Token_Type::semicolon)) {
                return a;
            }
        }
        return Grammar_Rule::assignment_statement;
    }

    Rule_Result match_assignment()
    {
        if (const Token* id = expect(Token_Type::identifier)) {
            if (expect(Token_Type::assign)) {
                if (Rule_Result e = match_expression()) {
                    return Node { *id, Node_Type::assignment,
                                  Assignment_Data { id->extract(m_source), std::move(*e) } };
                }
            }
        }
        return Grammar_Rule::assignment;
    }

    Rule_Result match_return_statement()
    {
        if (const Token* t = expect(Token_Type::keyword_return)) {
            if (Rule_Result e = match_expression()) {
                if (expect(Token_Type::semicolon)) {
                    return Node { *t, Node_Type::return_statement,
                                  Return_Statement_Data { std::move(*e) } };
                }
            }
        }
        return Grammar_Rule::return_statement;
    }

    Rule_Result match_break_statement()
    {
        if (const Token* t = expect(Token_Type::keyword_break)) {
            if (expect(Token_Type::semicolon)) {
                return Node { *t, Node_Type::break_statement };
            }
        }
        return Grammar_Rule::break_statement;
    }

    Rule_Result match_continue_statement()
    {
        if (const Token* t = expect(Token_Type::keyword_continue)) {
            if (expect(Token_Type::semicolon)) {
                return Node { *t, Node_Type::continue_statement };
            }
        }
        return Grammar_Rule::continue_statement;
    }

    Rule_Result match_if_or_while(Token_Type if_or_while)
    {
        BIT_MANIPULATION_ASSERT(if_or_while == Token_Type::keyword_if
                                || if_or_while == Token_Type::keyword_while);

        if (const Token* first = expect(if_or_while)) {
            if (expect(Token_Type::left_parenthesis)) {
                if (Rule_Result condition = match_expression()) {
                    if (expect(Token_Type::right_parenthesis)) {
                        if (Rule_Result block = match_block_statement()) {
                            const auto type = if_or_while == Token_Type::keyword_if
                                ? Node_Type::if_statement
                                : Node_Type::while_statement;
                            return Node { *first, type,
                                          If_While_Statement_Data { std::move(*condition),
                                                                    std::move(*block) } };
                        }
                    }
                }
            }
            return Grammar_Rule::block_statement;
        }
        return if_or_while == Token_Type::keyword_if ? Grammar_Rule::if_statement
                                                     : Grammar_Rule::while_statement;
    }

    Rule_Result match_for_statement()
    {
        if (const Token* first = expect(Token_Type::keyword_for)) {
            if (expect(Token_Type::left_parenthesis)) {
                if (Rule_Result init = match_init_clause()) {
                    if (expect(Token_Type::semicolon)) {
                        if (Rule_Result condition = match_expression()) {
                            if (expect(Token_Type::semicolon)) {
                                if (Rule_Result increment = expect(Grammar_Rule::assignment)) {
                                    if (expect(Token_Type::right_parenthesis)) {
                                        if (Rule_Result block = match_block_statement()) {
                                            return Node {
                                                *first, Node_Type::for_statement,
                                                For_Statement_Data {
                                                    std::move(*init), std::move(*condition),
                                                    std::move(*increment), std::move(*block) }
                                            };
                                        }
                                    }
                                }
                                else if (expect(Token_Type::right_parenthesis)) {
                                    if (Rule_Result block = match_block_statement()) {
                                        return Node { *first, Node_Type::for_statement,
                                                      For_Statement_Data { std::move(*init),
                                                                           std::move(*condition),
                                                                           std::move(*block) } };
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return Grammar_Rule::block_statement;
    }

    Rule_Result match_init_clause()
    {
        if (Rule_Result let = expect(Grammar_Rule::let_declaration)) {
            return let;
        }
        else if (Rule_Result assignment = match_assignment_statement()) {
            return assignment;
        }
        return Grammar_Rule::init_clause;
    }

    Rule_Result match_block_statement()
    {
        if (const Token* first = expect(Token_Type::left_brace)) {
            std::vector<Node> statements;
            while (Rule_Result s = match_statement()) {
                statements.push_back(std::move(*s));
            }
            if (expect(Token_Type::right_brace)) {
                return Node { *first, Node_Type::block_statement,
                              Block_Statement_Data { std::move(statements) } };
            }
        }
        return Grammar_Rule::block_statement;
    }

    Rule_Result match_expression()
    {
        if (Rule_Result if_expr = match_if_expression()) {
            return if_expr;
        }
        return Grammar_Rule::expression;
    }

    Rule_Result match_if_expression()
    {
        if (Rule_Result left = match_binary_expression()) {
            if (expect(Token_Type::keyword_if)) {
                if (Rule_Result condition = match_binary_expression()) {
                    if (expect(Token_Type::keyword_else)) {
                        if (Rule_Result right = match_binary_expression()) {
                            return Node { left->token, Node_Type::binary_expression,
                                          If_Expression_Data { std::move(*left),
                                                               std::move(*condition),
                                                               std::move(*right) } };
                        }
                    }
                }
            }
            return left;
        }
        return Grammar_Rule::if_expression;
    }

    Rule_Result match_binary_expression()
    {
        if (Rule_Result left = match_prefix_expression()) {
            {
                auto attempt = start_attempt();
                if (const Token* op = peek(); op && is_binary_operator(op->type)) {
                    attempt.commit();
                    pop();
                    if (Rule_Result right = match_prefix_expression()) {
                        return Node { left->token, Node_Type::binary_expression,
                                      Binary_Expression_Data { std::move(*left), std::move(*right),
                                                               op->type } };
                    }
                }
            }
            return left;
        }
        return Grammar_Rule::binary_expression;
    }

    Rule_Result match_prefix_expression()
    {
        {
            auto attempt = start_attempt();
            if (const Token* t = peek(); t && is_unary_operator(t->type)) {
                attempt.commit();
                pop();
                if (Rule_Result e = match_prefix_expression()) {
                    return Node { *t, Node_Type::prefix_expression,
                                  Prefix_Expression_Data { std::move(*e), t->type } };
                }
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
        if (const Token* t = expect(Token_Type::identifier)) {
            if (expect(Token_Type::left_parenthesis)) {
                if (expect(Token_Type::right_parenthesis)) {
                    return Node { *t, Node_Type::function_call_expression,
                                  Function_Call_Expression_Data { t->extract(m_source), {} } };
                }

                std::vector<Node> arguments;
                while (true) {
                    // Matching instead of expecting is correct here because the possibility of an
                    // empty parameter list has already been eliminated.
                    if (Rule_Result arg = match_expression()) {
                        arguments.push_back(std::move(*arg));
                        if (expect(Token_Type::comma)) {
                            continue;
                        }
                        else if (expect(Token_Type::right_parenthesis)) {
                            return Node { *t, Node_Type::function_call_expression,
                                          Function_Call_Expression_Data { t->extract(m_source),
                                                                          std::move(arguments) } };
                        }
                    }
                    return Grammar_Rule::expression_sequence;
                }
            }
        }
        return Grammar_Rule::function_call_expression;
    }

    Rule_Result match_primary_expression()
    {
        if (const Token* t = peek()) {
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
                if (Rule_Result e = match_expression()) {
                    if (expect(Token_Type::right_parenthesis)) {
                        return e;
                    }
                }
            }
        }
        return Grammar_Rule::primary_expression;
    }

    [[maybe_unused]] Rule_Result match_parenthesized_expression()
    {
        if (const Token* t = expect(Token_Type::left_parenthesis)) {
            if (Rule_Result e = match_expression()) {
                if (expect(Token_Type::right_parenthesis)) {
                    return e;
                }
            }
        }
        return Grammar_Rule::parenthesized_expression;
    }

    Rule_Result match_type()
    {
        if (const Token* t = peek()) {
            if (t->type == Token_Type::keyword_bool) {
                pop();
                return Node { *t, Node_Type::type, Type_Data::make_bool() };
            }
            if (t->type == Token_Type::keyword_int) {
                pop();
                return Node { *t, Node_Type::type, Type_Data::make_int() };
            }
            if (t->type == Token_Type::keyword_uint) {
                pop();
                if (expect(Token_Type::left_parenthesis)) {
                    if (Rule_Result node = match_expression()) {
                        if (expect(Token_Type::right_parenthesis)) {
                            return Node { *t, Node_Type::type,
                                          Type_Data::make_uint(std::move(*node)) };
                        }
                    }
                }
            }
        }
        return Grammar_Rule::type;
    }
};

} // namespace

Parse_Result parse(std::span<const Token> tokens, std::string_view source)
{
    return Parser(tokens, source).parse();
}

} // namespace bit_manipulation
