#include <optional>

#include "common/assert.hpp"
#include "common/parse.hpp"

#include "bmd/directive_type.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation::bmd {

namespace ast {

Content::Content(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children)
    : detail::Base { pos }
    , m_children(std::move(children))
{
    for (const auto* const child : m_children) {
        BIT_MANIPULATION_ASSERT(child != nullptr);
        BIT_MANIPULATION_ASSERT(std::holds_alternative<ast::Paragraph>(*child));
    }
}

Paragraph::Paragraph(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children)
    : detail::Base { pos }
    , m_children(std::move(children))
{
    for (const auto* const child : m_children) {
        BIT_MANIPULATION_ASSERT(child != nullptr);
        BIT_MANIPULATION_ASSERT(std::holds_alternative<ast::Directive>(*child)
                                || std::holds_alternative<ast::Text>(*child));
    }
}

Directive::Directive(const Local_Source_Span& pos,
                     Directive_Type type,
                     std::string_view identifier,
                     Arguments&& args,
                     ast::Some_Node* block)
    : detail::Base { pos }
    , m_type(type)
    , m_identifier(identifier)
    , m_arguments(std::move(args))
    , m_block(block)
{
    BIT_MANIPULATION_ASSERT(!identifier.empty());
}

Text::Text(const Local_Source_Span& pos, std::string_view text)
    : detail::Base { pos }
    , m_text(text)
{
    BIT_MANIPULATION_ASSERT(!text.empty());
}

Identifier::Identifier(const Local_Source_Span& pos, std::string_view value)
    : detail::Base { pos }
    , m_value(value)
{
    BIT_MANIPULATION_ASSERT(!value.empty());
}

Number::Number(const Local_Source_Span& pos, Int64 value, Literal_Type type)
    : detail::Base { pos }
    , m_value(value)
    , m_type(type)
{
}

Raw::Raw(const Local_Source_Span& pos, std::string_view value)
    : detail::Base { pos }
    , m_value(value)
{
    BIT_MANIPULATION_ASSERT(!value.empty());
}

} // namespace ast

namespace {

constexpr std::string_view after_number_characters = "[]{}=, \r\t\n";

Grammar_Rule grammar_rule_of(Literal_Type literal)
{
    using enum Grammar_Rule;
    switch (literal) {
    case Literal_Type::binary: return binary_literal;
    case Literal_Type::octal: return octal_literal;
    case Literal_Type::decimal: return decimal_literal;
    case Literal_Type::hexadecimal: return hexadecimal_literal;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown literal type.");
}

struct Blank {
    Size length;
    bool is_paragraph_break;
};

struct Parser {
private:
    std::string_view m_source;
#if 0
    std::pmr::vector<ast::Some_Node*> m_destroy_on_rollback;
#endif
    std::pmr::memory_resource* m_memory;
    Local_Source_Span m_pos = {};

    struct Rule_Error {
        Parse_Error_Code code;
        Grammar_Rule rule;
    };

    enum struct Blank_Type {
        /// Blank space between words.
        /// Whitespace or comment space which contains no empty line.
        word_break,
        /// Whitespace containing at least one empty line outside of block comments.
        paragraph_break,
    };

public:
    Parser(std::string_view source, std::pmr::memory_resource* memory)
        : m_source(source)
        , m_memory(memory)
    {
    }

    Result<Parsed_Document, Parse_Error> parse()
    {
        Result<ast::Some_Node*, Rule_Error> result = match_document();
        if (!result) {
            return Parse_Error { result.error().code, result.error().rule, m_pos };
        }
        return Parsed_Document { m_source, *result };
    }

private:
    // UTILITIES ===================================================================================

    void advance_position_by(Size length)
    {
        BIT_MANIPULATION_ASSERT(m_pos.begin + length < m_source.length());
        for (Size i = 0; i < length; ++i) {
            advance_position_based_on(m_source[m_pos.begin]);
        }
    }

    char advance_position_based_on(char c)
    {
        switch (c) {
        case '\t': m_pos.column += 4; break;
        case '\r': m_pos.column = 0; break;
        case '\n':
            m_pos.column = 0;
            m_pos.line += 1;
            break;
        default: m_pos.column += 1;
        }
        m_pos.begin += 1;
        return c;
    }

    /// @brief Returns all remaining text as a `std::string_view`, from the current parsing
    /// position to the end of the file.
    /// @return All remaining text.
    std::string_view peek_all() const
    {
        return m_source.substr(m_pos.begin);
    }

    /// @brief Returns the next character and advances the parser position.
    /// @return The popped character.
    /// @throws Throws if `eof()`.
    char pop()
    {
        return advance_position_based_on(peek());
    }

    /// @brief Returns the next character.
    /// @return The next character.
    /// @throws Throws if `eof()`.
    char peek()
    {
        BIT_MANIPULATION_ASSERT(!eof());
        return m_source[m_pos.begin];
    }

    /// @return `true` if the parser is at the end of the file, `false` otherwise.
    bool eof() const
    {
        return m_pos.begin == m_source.length();
    }

    /// @return `peek_all().starts_with(text)`.
    bool peek(std::string_view text) const
    {
        return peek_all().starts_with(text);
    }

    /// @brief Checks whether the next character matches an expected value without advancing
    /// the parser.
    /// @param c the character to test
    /// @return `true` if the next character equals `c`, `false` otherwise.
    bool peek(char c) const
    {
        return !eof() && m_source[m_pos.begin] == c;
    }

    /// @brief Checks whether the next character satisfies a predicate without advancing
    /// the parser.
    /// @param predicate the predicate to test
    /// @return `true` if the next character satisfies `predicate`, `false` otherwise.
    bool peek(bool predicate(char)) const
    {
        return !eof() && predicate(m_source[m_pos.begin]);
    }

    Result<void, Parse_Error_Code> expect(char c)
    {
        if (eof()) {
            return Parse_Error_Code::unexpected_eof;
        }
        if (m_source[m_pos.begin] != c) {
            return Parse_Error_Code::unexpected_character;
        }
        advance_position_based_on(c);
        return {};
    }

    Result<void, Parse_Error_Code> expect(bool predicate(char))
    {
        if (eof()) {
            return Parse_Error_Code::unexpected_eof;
        }
        if (!predicate(m_source[m_pos.begin])) {
            return Parse_Error_Code::unexpected_character;
        }
        advance_position_based_on(m_source[m_pos.begin]);
        return {};
    }

    bool expect(std::string_view text)
    {
        if (peek(text)) {
            advance_position_by(text.length());
            return true;
        }
        return false;
    }

#if 0
    Result<ast::Some_Node*, Rule_Error>
    expect(Result<ast::Some_Node*, Rule_Error> (Parser::*match)())
    {
        const auto restore_pos = m_pos;
        m_destroy_on_rollback.clear();
        const auto result = (this->*match)();
        if (!result) {
            m_pos = restore_pos;
            for (ast::Some_Node* node : m_destroy_on_rollback) {
                m_memory->deallocate(node, sizeof(ast::Some_Node), alignof(ast::Some_Node));
            }
        }
        return result;
    }
#endif

    template <typename T, typename... Args>
    ast::Some_Node* alloc_node(const Local_Source_Span& span, Args&&... args) const
    {
        void* storage = m_memory->allocate(sizeof(ast::Some_Node), alignof(ast::Some_Node));
        ast::Some_Node* result
            = new (storage) ast::Some_Node { T(span, std::forward<Args>(args)...) };
#if 0
        m_destroy_on_rollback.push_back(result);
#endif
        return result;
    }

    // RULE MATCHING ===============================================================================

    Result<ast::Some_Node*, Rule_Error> match_document()
    {
        auto blank = match_blank();
        if (!blank) {
            return Rule_Error { blank.error(), Grammar_Rule::blank };
        }
        if (eof()) {
            return nullptr;
        }
        return match_content();
    }

    Result<ast::Some_Node*, Rule_Error> match_content()
    {
        const auto initial_pos = m_pos;
        std::pmr::vector<ast::Some_Node*> paragraphs(m_memory);
        auto first = match_paragraph();
        if (!first) {
            return first;
        }
        paragraphs.push_back(std::move(*first));

        while (!eof()) {
            // After each parsed paragraph, we are either at the EOF,
            // or there is a trailing paragraph break.
            // Any trailing characters or regular blanks would have been absorbed into the
            // paragraph.
            auto paragraph_break = match_blank(Grammar_Rule::paragraph_break);
            if (!paragraph_break) {
                return paragraph_break.error();
            }
            BIT_MANIPULATION_ASSERT(paragraph_break->length != 0);

            // The break which we have just matched could be at the end of file.
            // Trailing paragraph breaks are allowed; we are done then.
            if (eof()) {
                break;
            }
            auto paragraph = match_paragraph();
            if (!paragraph) {
                break;
            }
            paragraphs.push_back(*paragraph);
        }
        return alloc_node<ast::Content>({ initial_pos, m_pos.begin - initial_pos.begin },
                                        std::move(paragraphs));
    }

    Result<ast::Some_Node*, Rule_Error> match_paragraph()
    {
        const auto initial_pos = m_pos;
        std::pmr::vector<ast::Some_Node*> elements(m_memory);
        while (!eof()) {
            if (peek(is_space)) {
                Result<Blank, Rule_Error> blank = match_blank(Grammar_Rule::blank);
                if (!blank) {
                    return blank.error();
                }
                // If there is a leading space but we couldn't match it as a blank, we must have run
                // into a paragraph break and should break.
                if (blank->length == 0) {
                    break;
                }
            }
            ast::Some_Node* text = match_text();
            if (text) {
                elements.push_back(text);
                continue;
            }
            if (peek('\\')) {
                auto directive = match_directive();
                if (!directive) {
                    return directive;
                }
                elements.push_back(*directive);
            }
        }
        return alloc_node<ast::Paragraph>({ initial_pos, m_pos.begin - initial_pos.begin },
                                          std::move(elements));
    }

    Result<ast::Some_Node*, Rule_Error> match_directive()
    {
        constexpr auto this_rule = Grammar_Rule::directive;
        const auto initial_pos = m_pos;

        if (auto r = expect('\\'); !r) {
            return Rule_Error { r.error(), this_rule };
        }
        Result identifier = match_identifier();
        if (!identifier) {
            return identifier.error();
        }
        std::optional<Directive_Type> type = directive_type_by_id(identifier->get_value());
        if (!type) {
            m_pos = m_pos.to_left(identifier->get_value().length());
            return Rule_Error { Parse_Error_Code::invalid_directive, this_rule };
        }

        ast::Directive::Arguments args(m_memory);
        if (peek('[')) {
            auto r = match_arguments();
            if (!r) {
                return r.error();
            }
            args = *r;
        }
        ast::Some_Node* block = nullptr;
        if (peek('{')) {
            if (identifier->get_value().starts_with("code")) {
                auto r = match_raw_block();
                if (!r) {
                    return r;
                }
                block = *r;
            }
            else {
                auto r = match_block();
                if (!r) {
                    return r;
                }
                block = *r;
            }
        }
        return alloc_node<ast::Directive>({ initial_pos, m_pos.begin - initial_pos.begin }, *type,
                                          identifier->get_value(), std::move(args), block);
    }

    Result<ast::Directive::Arguments, Rule_Error> match_arguments()
    {
        constexpr auto this_rule = Grammar_Rule::arguments;
        if (auto r = expect('['); !r) {
            return Rule_Error { r.error(), this_rule };
        }
        ast::Directive::Arguments arguments(m_memory);
        while (true) {
            if (auto r = match_blank(Grammar_Rule::blank); !r) {
                return r.error();
            }
            // restoration is solely for diagnostics quality
            auto restore_pos = m_pos;

            Result<ast::Identifier, Rule_Error> identifier = match_identifier();
            if (!identifier) {
                return identifier.error();
            }
            if (auto r = match_blank(Grammar_Rule::blank); !r) {
                return r.error();
            }
            if (auto r = expect('='); !r) {
                return Rule_Error { r.error(), Grammar_Rule::argument };
            }
            if (auto r = match_blank(Grammar_Rule::blank); !r) {
                return r.error();
            }
            Result<ast::Value, Rule_Error> value = match_value();
            if (!value) {
                return value.error();
            }
            auto [iter, success] = arguments.emplace(identifier->get_value(), *value);
            if (!success) {
                m_pos = restore_pos;
                return Rule_Error { Parse_Error_Code::duplicate_argument, Grammar_Rule::argument };
            }
            if (auto r = match_blank(Grammar_Rule::blank); !r) {
                return r.error();
            }
            if (eof()) {
                return Rule_Error { Parse_Error_Code::unexpected_eof, Grammar_Rule::argument };
            }
            if (!expect(',')) {
                break;
            }
        }
        if (auto r = expect(']'); !r) {
            return Rule_Error { r.error(), this_rule };
        }
        return arguments;
    }

    ast::Some_Node* match_text()
    {
        const Local_Source_Position initial_pos = m_pos;
        std::string_view text = match_text_impl();
        if (text.empty()) {
            return nullptr;
        }
        BIT_MANIPULATION_ASSERT(!is_space(text.front()));
        BIT_MANIPULATION_ASSERT(!is_space(text.back()));
        return alloc_node<ast::Text>({ initial_pos, text.length() }, text);
    }

    std::string_view match_text_impl()
    {
        const auto initial_pos = m_pos;
        auto final_pos = m_pos;
        bool paragraph_break_possible = false;

        while (!eof()) {
            char c = peek();
            if (is_space(c)) {
                if (c == '\n') {
                    if (paragraph_break_possible) {
                        break;
                    }
                    paragraph_break_possible = true;
                }
                pop();
                continue;
            }

            c = pop();
            if (c == '/') {
                if (peek('/') || peek('*')) {
                    break;
                }
            }
            if (c == '\\') {
                if (!expect('\\')) {
                    break;
                }
            }
            final_pos = m_pos;
            paragraph_break_possible = false;
        }

        m_pos = final_pos;
        return m_source.substr(initial_pos.begin, final_pos.begin - initial_pos.begin);
    }

    /// @brief Matches the grammar rule `block` of the non-raw form.
    /// @return `ast::Block`.
    Result<ast::Some_Node*, Rule_Error> match_block()
    {
        const auto inner_pos = m_pos.to_right(1);
        Result<ast::Raw, Rule_Error> raw = match_raw_block_impl();
        if (!raw) {
            return raw.error();
        }
        const Size new_end = raw->get_source_position().end();
        const std::string_view restore_source
            = std::exchange(m_source, m_source.substr(0, new_end));

        m_pos = inner_pos;
        if (auto r = match_blank(); !r) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::block };
        }
        Result<ast::Some_Node*, Rule_Error> result = match_content();
        if (!result) {
            m_source = restore_source;
        }
        else {
            BIT_MANIPULATION_ASSERT(eof());
            m_source = restore_source;
            BIT_MANIPULATION_ASSERT(peek('}'));
            m_pos = m_pos.to_right(1);
        }
        return result;
    }

    /// @brief Matches a "raw block", which is of the form `{`, text, `}`.
    /// Basically, it matches any brace-enclosed content.
    /// @return `ast::Text` which excludes the enclosing braces.
    Result<ast::Some_Node*, Rule_Error> match_raw_block()
    {
        Result<ast::Raw, Rule_Error> raw = match_raw_block_impl();
        if (!raw) {
            return raw.error();
        }
        return alloc_node<ast::Text>(raw->get_source_position(), raw->get_value());
    }

    /// @brief Same as `match_raw_block`, but does not allocate any node yet.
    /// It merely yields the `Source_Span` and `std::string_view` representing the raw block content
    /// (excluding enclosing braces, which are matched, but not returned in the result).
    /// @return The contents of the raw block, or `Rule_Error`.
    Result<ast::Raw, Rule_Error> match_raw_block_impl()
    {
        if (auto r = expect('{'); !r) {
            return Rule_Error { r.error(), Grammar_Rule::block };
        }

        const auto initial_pos = m_pos;
        Size brace_level = 0;

    normal_state: {
        if (eof()) {
            return Rule_Error { Parse_Error_Code::unexpected_eof, Grammar_Rule::raw_content };
        }
        if (expect('}')) {
            if (brace_level-- == 0) {
                goto end;
            }
            goto normal_state;
        }
        const char c = pop();
        if (c == '{') {
            ++brace_level;
            goto normal_state;
        }
        if (c == '/' && expect('/')) {
            goto in_line_comment;
        }
        if (c == '/' && expect('*')) {
            goto in_block_comment;
        }
        goto normal_state;
    }

    in_line_comment: {
        if (eof()) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::raw_content };
        }
        const char c = pop();
        if (c == '\n') {
            goto normal_state;
        }
        goto in_line_comment;
    }

    in_block_comment: {
        if (eof()) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::raw_content };
        }
        char c = pop();
        if (c == '*' && expect('/')) {
            goto normal_state;
        }
        goto in_block_comment;
    }

    end:
        const Size length = m_pos.begin - initial_pos.begin - 1;
        return ast::Raw { { initial_pos, length }, m_source.substr(initial_pos.begin, length) };
    }

    Result<Blank, Rule_Error> match_blank(Grammar_Rule rule)
    {
        BIT_MANIPULATION_ASSERT(rule == Grammar_Rule::blank
                                || rule == Grammar_Rule::paragraph_break);

        const auto rollback_pos = m_pos;
        auto result = match_blank();
        if (!result) {
            return Rule_Error { result.error(), rule };
        }
        if (result->is_paragraph_break != (rule == Grammar_Rule::paragraph_break)) {
            m_pos = rollback_pos;
            return Blank { .length = 0, .is_paragraph_break = result->is_paragraph_break };
        }
        return *result;
    }

    Result<Blank, Parse_Error_Code> match_blank()
    {
        const Size initial_pos = m_pos.begin;

        Size newline_count = 0;
        bool is_paragraph_break = false;

    normal_state: {
        if (eof()) {
            goto end;
        }
        const char c = peek();
        if (c == '/') {
            if (expect("//")) {
                newline_count = 0;
                goto in_line_comment;
            }
            if (expect("/*")) {
                newline_count = 0;
                goto in_block_comment;
            }
        }
        if (!is_space(c)) {
            goto end;
        }
        pop();
        if (c == '\n') {
            is_paragraph_break |= ++newline_count > 1;
        }
        goto normal_state;
    }

    in_line_comment: {
        if (eof()) {
            return Parse_Error_Code::unterminated_comment;
        }
        const char c = pop();
        if (c == '\n') {
            newline_count = 1;
            goto normal_state;
        }
        goto in_line_comment;
    }

    in_block_comment: {
        if (eof()) {
            return Parse_Error_Code::unterminated_comment;
        }
        char c = pop();
        if (c == '*' && expect('/')) {
            goto normal_state;
        }
        goto in_block_comment;
    }

    end:
        return Blank { m_pos.begin - initial_pos, is_paragraph_break };
    }

    Result<ast::Value, Rule_Error> match_value()
    {
        if (peek(is_decimal_digit)) {
            Result<ast::Number, Rule_Error> literal = match_literal();
            if (!literal) {
                return literal.error();
            }
            return ast::Value { *literal };
        }
        else {
            Result<ast::Identifier, Rule_Error> identifier = match_identifier();
            if (!identifier) {
                return identifier.error();
            }
            return ast::Value { *identifier };
        }
    }

    Result<ast::Number, Rule_Error> match_literal()
    {
        if (eof()) {
            return Rule_Error { Parse_Error_Code::unexpected_eof, Grammar_Rule::decimal_literal };
        }

        const std::string_view s = peek_all();
        const Literal_Match_Result literal_match = match_integer_literal(s);
        if (literal_match.status == Literal_Match_Status::no_digits) {
            return Rule_Error { Parse_Error_Code::invalid_integer_literal,
                                Grammar_Rule::decimal_literal };
        }

        const auto rule = grammar_rule_of(literal_match.type);
        if (literal_match.status == Literal_Match_Status::no_digits_following_prefix) {
            m_pos = m_pos.to_right(literal_match.length);
            return Rule_Error { Parse_Error_Code::invalid_integer_literal, rule };
        }

        const bool attempted_integer_suffix = s.length() > literal_match.length
            && identifier_characters.find(s[literal_match.length]) != std::string_view::npos;
        if (attempted_integer_suffix) {
            m_pos = m_pos.to_right(literal_match.length);
            return Rule_Error { Parse_Error_Code::integer_suffix, rule };
        }

        std::optional<Big_Int> value = parse_integer_literal(s.substr(0, literal_match.length));
        if (!value) {
            return Rule_Error { Parse_Error_Code::invalid_integer_literal,
                                Grammar_Rule::decimal_literal };
        }
        const auto initial_pos = std::exchange(m_pos, m_pos.to_right(literal_match.length));
        return ast::Number { { initial_pos, literal_match.length },
                             Int64(*value),
                             literal_match.type };
    }

    Result<ast::Identifier, Rule_Error> match_identifier()
    {
        if (eof()) {
            return Rule_Error { Parse_Error_Code::unexpected_eof, Grammar_Rule::identifier };
        }
        const Size length = bit_manipulation::match_identifier(peek_all());
        if (length == 0) {
            return Rule_Error { Parse_Error_Code::unexpected_character, Grammar_Rule::identifier };
        }
        const auto initial_pos = std::exchange(m_pos, m_pos.to_right(length));
        return ast::Identifier { { initial_pos, length },
                                 m_source.substr(initial_pos.begin, length) };
    }
};

} // namespace

Result<Parsed_Document, Parse_Error> parse(std::string_view source,
                                           std::pmr::memory_resource* memory)
{
    return Parser { source, memory }.parse();
}

} // namespace bit_manipulation::bmd
