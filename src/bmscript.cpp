#include <optional>
#include <variant>

#include "assert.hpp"
#include "bmscript.hpp"

using namespace bit_manipulation::ast;

namespace bit_manipulation {

namespace {

#if 0
enum struct Parse_Exception {
    unknown_token //
};
#endif

constexpr std::string_view identifier_characters = "abcdefghijklmnopqrstuvwxyz"
                                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                                   "0123456789_";

constexpr bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

std::optional<Token_Type> keyword_by_name(std::string_view s)
{
    using enum Token_Type;

    if (s == "if") {
        return keyword_if;
    }
    if (s == "for") {
        return keyword_for;
    }
    if (s == "let") {
        return keyword_let;
    }
    if (s == "Int") {
        return keyword_int;
    }
    if (s == "else") {
        return keyword_else;
    }
    if (s == "Uint") {
        return keyword_uint;
    }
    if (s == "Int") {
        return keyword_int;
    }
    if (s == "Bool") {
        return keyword_bool;
    }
    if (s == "const") {
        return keyword_const;
    }
    if (s == "break") {
        return keyword_break;
    }
    if (s == "while") {
        return keyword_while;
    }
    if (s == "function") {
        return keyword_function;
    }
    if (s == "requires") {
        return keyword_requires;
    }
    if (s == "continue") {
        return keyword_continue;
    }
    return std::nullopt;
}

std::optional<Token_Type> try_identify_fixed_length_token(std::string_view s)
{
    using enum Token_Type;

    if (s.empty()) {
        return std::nullopt;
    }
    switch (s[0]) {
    case '(': return left_parenthesis;
    case ')': return right_parenthesis;
    case '{': return left_brace;
    case '}': return right_brace;
    case '=': {
        if (s.length() > 1) {
            if (s[1] == '=') {
                return equals;
            }
            if (s[1] == '>') {
                return double_right_arrow;
            }
        }
        return assign;
    }

    case '+': return plus;
    case '-': {
        if (s.length() > 1 && s[1] == '>') {
            return right_arrow;
        }
        return minus;
    }
    case '*': {
        // comments are already handled elsewhere, so this is always multiplication
        return multiplication;
    }
    case '/':
        // comments are already handled elsewhere, so this is always division
        return division;
    case '%': return remainder;
    case '<': {
        if (s.length() > 1) {
            if (s[1] == '<') {
                return shift_left;
            }
            if (s[1] == '=') {
                return less_or_equal;
            }
            return less_than;
        }
    }
    case '>': {
        if (s.length() > 1) {
            if (s[1] == '>') {
                return shift_right;
            }
            if (s[1] == '=') {
                return greater_or_equal;
            }
        }
        return greater_than;
    }
    case '&': {
        if (s.length() > 1 && s[1] == '&') {
            return logical_and;
        }
        return bitwise_and;
    }
    case '|': {
        if (s.length() > 1 && s[1] == '|') {
            return logical_or;
        }
        return bitwise_or;
    }
    case '^': {
        return bitwise_xor;
    }
    case '~': {
        return bitwise_not;
    }
    case '!': {
        if (s.length() > 1 && s[1] == '=') {
            return not_equals;
        }
        return logical_not;
    }
    case ':': {
        return colon;
    }
    case ';': {
        return semicolon;
    }
    case '.': {
        return dot;
    }
    case ',': {
        return comma;
    }
    }
    return std::nullopt;
}

struct Tokenize_Result {
    Size length;
    Token_Type type;

    [[nodiscard]] explicit operator bool() const
    {
        return length != 0;
    }
};

Tokenize_Result try_tokenize_literal(std::string_view s)
{
    if (s.empty() || !is_digit(s[0])) {
        return {};
    }
    if (s[0] == '0') {
        if (s.length() > 1) {
            if (s[1] == 'x' || s[1] == 'b') {
                const Token_Type type
                    = s[1] == 'x' ? Token_Type::hexadecimal_literal : Token_Type::binary_literal;
                const std::string_view allowed_digits
                    = s[1] == 'x' ? "0123456789abcdefABCDEF" : "01";
                const Size digits = s.substr(2).find_first_not_of(allowed_digits);
                const Size result = digits == 0        ? 0
                    : digits == std::string_view::npos ? s.length()
                                                       : digits + 2;
                return { result, type };
            }
            const Size digits = s.find_first_not_of("01234567");
            return { std::min(digits, s.length()), Token_Type::octal_literal };
        }
        return { 1, Token_Type::decimal_literal };
    }
    const Size digits = s.find_first_not_of("0123456789");
    const Size result = digits == 0 ? 0 : digits == std::string_view::npos ? s.length() : digits;
    return { result, Token_Type::decimal_literal };
}

Size try_tokenize_identifier(std::string_view str)
{
    if (str.empty() || is_digit(str[0])) {
        return 0;
    }
    const Size result = str.find_first_not_of(identifier_characters);
    return std::min(result, str.length());
}

Tokenize_Result try_tokenize_identifier_or_keyword(std::string_view str)
{
    const Size identifier_length = try_tokenize_identifier(str);
    if (identifier_length == 0) {
        return { 0, Token_Type::identifier };
    }
    if (std::optional<Token_Type> keyword = keyword_by_name(str.substr(0, identifier_length))) {
        return { identifier_length, *keyword };
    }
    return { identifier_length, Token_Type::identifier };
}

std::optional<Tokenize_Result> try_tokenize(std::string_view s)
{
    if (s.starts_with("//")) {
        const Size length = std::min(s.find('\n', 2), s.length());
        return Tokenize_Result { length, Token_Type::line_comment };
    }
    if (s.starts_with("/*")) {
        // naive: nesting disallowed, but line comments can be nested in block comments
        const Size end = s.find("*/", 2);
        if (end != std::string_view::npos) {
            return Tokenize_Result { end + 2, Token_Type::block_comment };
        }
        // An unclosed comment should count as a tokenization failure.
        return std::nullopt;
    }
    if (const Tokenize_Result r = try_tokenize_identifier_or_keyword(s)) {
        return Tokenize_Result { r.length, r.type };
    }
    if (const Tokenize_Result r = try_tokenize_literal(s)) {
        return Tokenize_Result { r.length, r.type };
    }
    if (const std::optional<Token_Type> type = try_identify_fixed_length_token(s)) {
        return Tokenize_Result { token_type_length(*type), *type };
    }
    return std::nullopt;
}

Source_Position advance_position_by_text(Source_Position pos, std::string_view text)
{
    for (char c : text) {
        switch (c) {
        case '\t': pos.column += 4; break;
        case '\r': pos.column = 0; break;
        case '\n':
            pos.column = 0;
            pos.line += 1;
            break;
        default: pos.column += 1;
        }
        pos.begin += 1;
    }
    return pos;
}

} // namespace

Tokenize_Error tokenize(std::vector<Token>& out, std::string_view source) noexcept
{
    Source_Position pos {};

    while (true) {
        std::string_view remainder = source.substr(pos.begin);
        if (remainder.empty()) {
            break;
        }
        if (const Size whitespace_length = remainder.find_first_not_of(" \r\t\n")) {
            if (whitespace_length == std::string_view::npos) {
                break;
            }
            pos = advance_position_by_text(pos, remainder.substr(0, whitespace_length));
            remainder = remainder.substr(whitespace_length);
        }

        if (std::optional<Tokenize_Result> part = try_tokenize(remainder)) {
            out.push_back({ pos, part->length, part->type });
            pos.begin += part->length;
            pos.column += part->length;
        }
        else {
            return { Tokenize_Error_Code::illegal_character, pos };
        }
    }

    return {};
}

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

    const Grammar_Rule get_error() const
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
        Size pos;

        Attempt(Parser& parser, Size pos)
            : parser { parser }
            , pos { pos }
        {
        }

        ~Attempt()
        {
            parser.m_pos = pos;
        }

        void commit()
        {
            pos = parser.m_pos;
        }
    };

public:
    explicit Parser(std::span<const Token> tokens, std::string_view source)
        : m_tokens { tokens }
        , m_source { source }
        , m_pos { 0 }
    {
    }

private:
    const Token* try_peek() const
    {
        return m_pos < m_tokens.size() ? &m_tokens[m_pos] : nullptr;
    }

    const Token* try_pop()
    {
        return m_pos < m_tokens.size() ? &m_tokens[m_pos++] : nullptr;
    }

    const Token* next_equals(Token_Type type) const
    {
        if (const Token* next = try_peek(); next && next->type == type) {
            return next;
        }
        return nullptr;
    }

    const Token* expect(Token_Type type)
    {
        if (const Token* next = try_pop(); next && next->type == type) {
            return next;
        }
        return nullptr;
    }

    const Token* advance_if_next_equals(Token_Type type)
    {
        if (const Token* next = try_peek(); next && next->type == type) {
            m_pos += 1;
            return next;
        }
        return nullptr;
    }

    /*
    void advance()
    {
        BIT_MANIPULATION_ASSERT(m_pos < m_tokens.size());
        m_pos += 1;
    }*/

    Attempt start_attempt()
    {
        return { *this, m_pos };
    }

    Rule_Result match_rule(Grammar_Rule rule)
    {
        switch (rule) {
        case Grammar_Rule::let_declaration: return match_variable(Token_Type::keyword_let);
        case Grammar_Rule::const_declaration: return match_variable(Token_Type::keyword_const);
        case Grammar_Rule::assignment_statement: return match_assignment_statement();
        case Grammar_Rule::break_statement: return match_break_statement();
        case Grammar_Rule::continue_statement: return match_continue_statement();
        case Grammar_Rule::return_statement: return match_return_statement();
        case Grammar_Rule::if_statement: return match_if_or_while(Token_Type::keyword_if);
        case Grammar_Rule::while_statement: return match_if_or_while(Token_Type::keyword_while);
        case Grammar_Rule::for_statement: return match_for_statement();
        case Grammar_Rule::block_statement: return match_block_statement();
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    Node match_program()
    {
        std::vector<Node> declarations;
        while (m_pos != m_tokens.size()) {
            // declarations.push_back(match_declaration());
        }
    }

    Rule_Result match_statement(Token_Type const_or_let)
    {
        static constexpr Grammar_Rule rules[] {
            Grammar_Rule::let_declaration,      Grammar_Rule::const_declaration,
            Grammar_Rule::assignment_statement, Grammar_Rule::break_statement,
            Grammar_Rule::continue_statement,   Grammar_Rule::return_statement,
            Grammar_Rule::if_statement,         Grammar_Rule::while_statement,
            Grammar_Rule::for_statement,        Grammar_Rule::block_statement
        };

        for (Size i = 0; i + 1 < std::size(rules); ++i) {
            auto attempt = start_attempt();
            if (Rule_Result r = match_rule(rules[i])) {
                return r;
            }
        }

        return match_rule(rules[std::size(rules) - 1]);
    }

    Rule_Result match_variable(Token_Type const_or_let)
    {
        BIT_MANIPULATION_ASSERT(const_or_let == Token_Type::keyword_const
                                || const_or_let == Token_Type::keyword_let);

        if (const Token* t = expect(const_or_let)) {
            if (const Token* id = expect(Token_Type::identifier)) {
                const std::string_view name = id->extract(m_source);

                if (advance_if_next_equals(Token_Type::colon)) {
                    if (Rule_Result type = match_type()) {
                        {
                            auto attempt = start_attempt();
                            if (Rule_Result init = match_initializer()) {
                                if (expect(Token_Type::semicolon)) {
                                    attempt.commit();
                                    return Node { t->pos, Node_Type::variable,
                                                  Let_Const_Data::type_and_initializer(
                                                      const_or_let, name, std::move(*type),
                                                      std::move(*init)) };
                                }
                            }
                        }
                        if (expect(Token_Type::semicolon)
                            && const_or_let == Token_Type::keyword_let) {
                            return Node { t->pos, Node_Type::variable,
                                          Let_Const_Data::let_type_only(name, std::move(*type)) };
                        }
                    }
                    else {
                        return Grammar_Rule::type;
                    }
                }
                else if (Rule_Result init = match_initializer()) {
                    if (expect(Token_Type::semicolon)) {
                        return Node { t->pos, Node_Type::variable,
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
        return Grammar_Rule::initializer;
    }

    Rule_Result match_statement()
    {
        return Grammar_Rule::statement;
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
                    return Node { id->pos, Node_Type::assignment,
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
                    return Node { t->pos, Node_Type::return_statement,
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
                return Node { t->pos, Node_Type::break_statement };
            }
        }
        return Grammar_Rule::break_statement;
    }

    Rule_Result match_continue_statement()
    {
        if (const Token* t = expect(Token_Type::keyword_continue)) {
            if (expect(Token_Type::semicolon)) {
                return Node { t->pos, Node_Type::continue_statement };
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
                            return Node { first->pos, type,
                                          If_While_Statement_Data { std::move(*condition),
                                                                    std::move(*block) } };
                        }
                    }
                }
            }
            return Grammar_Rule::block_statement;
        }
    }

    Rule_Result match_for_statement()
    {
        if (const Token* first = expect(Token_Type::keyword_for)) {
            if (expect(Token_Type::left_parenthesis)) {
                if (Rule_Result init = match_init_clause()) {
                    if (expect(Token_Type::semicolon)) {
                        if (Rule_Result condition = match_expression()) {
                            if (Rule_Result increment = match_assignment()) {
                                if (expect(Token_Type::right_parenthesis)) {
                                    if (Rule_Result block = match_block_statement()) {
                                        return Node { first->pos, Node_Type::for_statement,
                                                      For_Statement_Data { std::move(*init),
                                                                           std::move(*condition),
                                                                           std::move(*increment),
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
        if (Rule_Result let = match_let_declaration()) {
            return let;
        }
        if (Rule_Result assignment = match_assignment_statement()) {
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
                return Node { first->pos, Node_Type::block_statement,
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
            {
                auto attempt = start_attempt();
                if (expect(Token_Type::keyword_if)) {
                    if (Rule_Result condition = match_binary_expression()) {
                        if (expect(Token_Type::keyword_else)) {
                            if (Rule_Result right = match_binary_expression()) {
                                attempt.commit();
                                return Node { left->pos, Node_Type::binary_expression,
                                              If_Expression_Data { std::move(*left),
                                                                   std::move(*condition),
                                                                   std::move(*right) } };
                            }
                        }
                    }
                }
                return left;
            }
        }
        return Grammar_Rule::if_expression;
    }

    Rule_Result match_binary_expression()
    {
        if (Rule_Result left = match_prefix_expression()) {
            {
                auto attempt = start_attempt();
                if (const Token* op = try_pop(); op && is_binary_operator(op->type)) {
                    if (Rule_Result right = match_prefix_expression()) {
                        attempt.commit();
                        return Node { left->pos, Node_Type::binary_expression,
                                      Binary_Expression_Data { std::move(*left), std::move(*right),
                                                               op->type } };
                    }
                }
            }
            return left;
        }
        return Grammar_Rule::binary_expression;
    }

    Rule_Result match_binary_expression()
    {
        if (Rule_Result left = match_prefix_expression()) {
            {
                auto attempt = start_attempt();
                if (const Token* op = try_pop(); op && is_binary_operator(op->type)) {
                    if (Rule_Result right = match_prefix_expression()) {
                        attempt.commit();
                        return Node { op->pos, Node_Type::binary_expression,
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
            if (const Token* t = try_pop(); t && is_unary_operator(t->type)) {
                if (Rule_Result e = match_prefix_expression()) {
                    attempt.commit();
                    return Node { t->pos, Node_Type::prefix_expression,
                                  Prefix_Expression_Data { std::move(*e), t->type } };
                }
            }
        }
        return match_postfix_expression();
    }

    Rule_Result match_postfix_expression()
    {
        {
            auto attempt = start_attempt();
            if (Rule_Result call = match_function_call_expression()) {
                attempt.commit();
                return call;
            }
        }
        return match_primary_expression();
    }

    Rule_Result match_function_call_expression()
    {
        if (const Token* t = expect(Token_Type::identifier)) {
            if (expect(Token_Type::left_parenthesis)) {
                if (advance_if_next_equals(Token_Type::right_parenthesis)) {
                    return Node { t->pos, Node_Type::function_call_expression,
                                  Function_Call_Expression_Data { t->extract(m_source), {} } };
                }

                std::vector<Node> arguments;
                while (true) {
                    if (Rule_Result arg = match_expression()) {
                        arguments.push_back(std::move(*arg));
                        if (advance_if_next_equals(Token_Type::comma)) {
                            continue;
                        }
                        else if (expect(Token_Type::right_parenthesis)) {
                            return Node { t->pos, Node_Type::function_call_expression,
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
        if (const Token* t = try_pop()) {
            if (is_literal(t->type)) {
                return Node { t->pos, Node_Type::literal };
            }
            if (t->type == Token_Type::identifier) {
                return Node { t->pos, Node_Type::id_expression };
            }
            if (t->type == Token_Type::left_parenthesis) {
                if (Rule_Result e = match_expression()) {
                    if (expect(Token_Type::right_parenthesis)) {
                        return e;
                    }
                }
            }
        }
        return Grammar_Rule::primary_expression;
    }

    Rule_Result match_type()
    {
        if (const Token* t = try_pop()) {
            if (t->type == Token_Type::keyword_bool) {
                return Node { t->pos, Node_Type::type, Type_Data::make_bool() };
            }
            if (t->type == Token_Type::keyword_int) {
                return Node { t->pos, Node_Type::type, Type_Data::make_int() };
            }
            if (t->type == Token_Type::keyword_uint) {
                if (expect(Token_Type::left_parenthesis)) {
                    if (Rule_Result node = match_expression()) {
                        if (expect(Token_Type::right_parenthesis)) {
                            return Node { t->pos, Node_Type::type,
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

Parse_Result parse(std::span<const Token> tokens) noexcept { }

} // namespace bit_manipulation
