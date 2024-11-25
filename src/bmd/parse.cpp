#include <optional>

#include "common/assert.hpp"
#include "common/parse.hpp"

#include "bmd/directive_type.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation::bmd {

namespace ast {

List::List(const Local_Source_Span& pos, std::pmr::vector<ast::Some_Node*>&& children)
    : detail::Base { pos }
    , m_children(std::move(children))
{
    for (const auto* const child : m_children) {
        BIT_MANIPULATION_ASSERT(child != nullptr);
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

bool is_escapeable(char c)
{
    return c == '\\' || c == '}' || c == '{';
}

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

    /// @brief Checks whether the parser is at the start of a directive.
    /// Namely, has to be `\\` and not be the start of an escape sequence such as `\\\\` for this
    /// to be the case.
    /// This function can have false positives in the sense that if the subsequent directive is
    /// ill-formed, the guess was optimistic, and there isn't actually a directive there.
    /// However, it has no false negatives.
    /// @return `true` if the parser is at the start of a directive, `false` otherwise.
    bool peek_possible_directive() const
    {
        const std::string_view rest = peek_all();
        return !rest.empty() //
            && rest[0] == '\\' //
            && (rest.length() <= 1 || !is_escapeable(rest[1]));
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
        constexpr auto this_rule = Grammar_Rule::content;

        const auto initial_pos = m_pos;

        std::pmr::vector<ast::Some_Node*> elements(m_memory);
        std::pmr::vector<ast::Some_Node*> paragraph_elements(m_memory);

        /// If `!paragraph_elements.empty()`, creates a new paragraph containing the elements
        /// and appends it to `elements`.
        /// Other state is update to make subsequent calls possible.
        auto break_paragraph =
            [this, &elements, &paragraph_elements, pos_before_paragraph = initial_pos]() mutable //
        {
            if (paragraph_elements.empty()) {
                return;
            }
            elements.push_back(alloc_node<ast::List>(
                { pos_before_paragraph, m_pos.begin - pos_before_paragraph.begin },
                std::vector(paragraph_elements)));
            paragraph_elements.clear();
            pos_before_paragraph = m_pos;
        };

        while (!eof()) {
            BIT_MANIPULATION_ASSERT(!peek(is_space));

            if (peek_possible_directive()) {
                auto directive_result = match_directive(Directive_Content_Type::block);
                if (!directive_result) {
                    return directive_result;
                }
                const auto pos_before_directive = m_pos;
                const auto& directive = get<ast::Directive>(**directive_result);

                switch (directive_type_environment(directive.get_type())) {

                case Directive_Environment::list:
                case Directive_Environment::meta: {
                    m_pos = pos_before_directive;
                    return Rule_Error { Parse_Error_Code::directive_not_allowed, this_rule };
                }
                case Directive_Environment::content: {
                    break_paragraph();
                    elements.push_back(*directive_result);
                    break;
                }
                case Directive_Environment::paragraph: {
                    paragraph_elements.push_back(*directive_result);
                    break;
                }
                }
            }
            else {
                ast::Some_Node* const text = match_text();
                // We aren't at the EOF, there is no leading whitespace, and we are not at a
                // directive.
                // Every possible reason for a match failure has been excluded; there must be text.
                BIT_MANIPULATION_ASSERT(text != nullptr);
                paragraph_elements.push_back(text);
            }

            Result<Blank, Parse_Error_Code> blank = match_blank();
            if (!blank) {
                return Rule_Error { blank.error(), this_rule };
            }
            if (blank->is_paragraph_break) {
                break_paragraph();
            }
        }

        break_paragraph();
        return alloc_node<ast::List>({ initial_pos, m_pos.begin - initial_pos.begin },
                                     std::move(elements));
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
                // If there is a leading space but we couldn't match it as a blank, we must have
                // run into a paragraph break and should break.
                if (blank->length == 0) {
                    break;
                }
            }
            ast::Some_Node* text = match_text();
            if (text) {
                elements.push_back(text);
                continue;
            }
            if (peek_possible_directive()) {
                auto directive = match_directive(Directive_Content_Type::span);
                if (!directive) {
                    return directive;
                }
                elements.push_back(*directive);
            }
        }
        return alloc_node<ast::List>({ initial_pos, m_pos.begin - initial_pos.begin },
                                     std::move(elements));
    }

    Result<ast::Some_Node*, Rule_Error> match_directive(Directive_Content_Type context)
    {
        // We could meaningfully implement this function for contexts that don't allow
        // directives, but this would be dead code. If the context doesn't allow directives, we
        // should call different functions and get more meaningful diagnostics.
        BIT_MANIPULATION_ASSERT(directive_content_allows_directives(context));

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
        if (!directive_type_allowed_in(*type, context)) {
            return Rule_Error { Parse_Error_Code::directive_not_allowed, this_rule };
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
            auto r = match_directive_content(directive_type_content_type(*type));
            if (!r) {
                return r;
            }
            block = *r;
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
                if (!expect(is_escapeable)) {
                    break;
                }
            }
            final_pos = m_pos;
            paragraph_break_possible = false;
        }

        m_pos = final_pos;
        return m_source.substr(initial_pos.begin, final_pos.begin - initial_pos.begin);
    }

    Result<ast::Some_Node*, Rule_Error> match_directive_content(Directive_Content_Type type)
    {
        switch (type) {
        case Directive_Content_Type::nothing: //
            return match_empty_block();
        case Directive_Content_Type::text_span:
        case Directive_Content_Type::span:
            return match_inside_block(&Parser::match_span_inside_raw_block, type);
        case Directive_Content_Type::block: //
            return match_inside_block(&Parser::match_block_inside_raw_block, type);
        case Directive_Content_Type::meta: //
        case Directive_Content_Type::list: //
            return match_inside_block(&Parser::match_directives_inside_raw_block, type);
        case Directive_Content_Type::raw: //
            return match_raw_block();
        }

        BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid directive content type.");
    }

    Result<ast::Some_Node*, Rule_Error> match_empty_block()
    {
        if (auto r = match_blank(); !r) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::block };
        }
        if (!expect('}')) {
            return Rule_Error { Parse_Error_Code::directive_must_be_empty, Grammar_Rule::block };
        }
        return nullptr;
    }

    Result<ast::Some_Node*, Rule_Error> match_span_inside_raw_block(Directive_Content_Type content)
    {
        const bool directives_allowed = directive_content_allows_directives(content);

        auto leading_blank = match_blank();
        if (!leading_blank) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::block };
        }
        if (leading_blank->is_paragraph_break) {
            return Rule_Error { Parse_Error_Code::paragraph_break_in_span, Grammar_Rule::block };
        }
        auto result = directives_allowed ? match_paragraph() : match_text();
        if (!result) {
            return result;
        }
        if (peek(is_space)) {
            // Text already consumes trailing space, so it only would have failed to consume
            // all if the trailing space is a paragraph break.
            return Rule_Error { Parse_Error_Code::paragraph_break_in_span, Grammar_Rule::block };
        }
        if (!directives_allowed && peek_possible_directive()) {
            return Rule_Error { Parse_Error_Code::directive_in_text_span, Grammar_Rule::block };
        }
        return result;
    }

    Result<ast::Some_Node*, Rule_Error> match_block_inside_raw_block(Directive_Content_Type type)
    {
        BIT_MANIPULATION_ASSERT(type == Directive_Content_Type::block);

        if (auto r = match_blank(); !r) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, Grammar_Rule::block };
        }
        return match_content();
    }

    Result<ast::Some_Node*, Rule_Error>
    match_directives_inside_raw_block(Directive_Content_Type type)
    {
        BIT_MANIPULATION_ASSERT(directive_content_allows_only_directives(type));

        constexpr auto this_rule = Grammar_Rule::block;

        const auto initial_pos = m_pos;

        if (auto r = match_blank(); !r) {
            return Rule_Error { Parse_Error_Code::unterminated_comment, this_rule };
        }

        std::pmr::vector<ast::Some_Node*> directives(m_memory);
        while (!eof()) {
            if (!peek_possible_directive()) {
                return Rule_Error { Parse_Error_Code::text_in_directive_list, this_rule };
            }
            auto directive = match_directive(type);
            if (!directive) {
                return directive;
            }

            if (auto r = match_blank(); !r) {
                return Rule_Error { Parse_Error_Code::unterminated_comment, this_rule };
            }
            directives.push_back(std::move(*directive));
        }
        return alloc_node<ast::List>({ initial_pos, m_pos.begin - initial_pos.begin },
                                     std::move(directives));
    }

    /// @brief Matches a raw block and then performs parsing through the given `match`
    /// member function pointer inside that raw block.
    /// From the perspective of the `match` function, the parser is one character past the
    /// opening
    /// `{`, and the closing `}` is artificially considered to be the end of the file.
    ///
    /// The given `match` function is required to consume the block contents entirely.
    /// From the perspective of `match`, `eof()` shall be a postcondition.
    /// @param match the given match member function pointer
    /// @param block_content the content type of the block; forwarded to the `match` function
    /// @return The result of the `match` function.
    Result<ast::Some_Node*, Rule_Error>
    match_inside_block(Result<ast::Some_Node*, Rule_Error> (Parser::*match)(Directive_Content_Type),
                       Directive_Content_Type block_content)
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

        Result<ast::Some_Node*, Rule_Error> result = (this->*match)(block_content);
        if (!result) {
            m_source = restore_source;
        }
        else {
            // The given match function must consume the contents of the block entirely.
            BIT_MANIPULATION_ASSERT(eof());
            m_source = restore_source;
            // Sanity check: eof() from the perspective of `match` is `}` from our perspective.
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
    /// It merely yields the `Source_Span` and `std::string_view` representing the raw block
    /// content (excluding enclosing braces, which are matched, but not returned in the result).
    /// @return The contents of the raw block, or `Rule_Error`.
    Result<ast::Raw, Rule_Error> match_raw_block_impl()
    {
        if (auto r = expect('{'); !r) {
            return Rule_Error { r.error(), Grammar_Rule::block };
        }

        const auto initial_pos = m_pos;
        Size brace_level = 0;

        while (true) {
            if (eof()) {
                return Rule_Error { Parse_Error_Code::unexpected_eof, Grammar_Rule::raw_content };
            }
            if (expect('}')) {
                if (brace_level-- == 0) {
                    break;
                }
                continue;
            }
            const char c = pop();
            // Braces are escapeable, so \{ doesn't contribute to the brace matching.
            if (c == '\\' && expect(is_escapeable)) {
                continue;
            }
            // ... but unescaped braces do.
            if (c == '{') {
                ++brace_level;
                continue;
            }
            // Parse C99-style line comments.
            // Braces inside line comments don't contribute to brace matching.
            if (c == '/' && expect('/')) {
                while (true) {
                    if (eof()) {
                        return Rule_Error { Parse_Error_Code::unterminated_comment,
                                            Grammar_Rule::raw_content };
                    }
                    const char c = pop();
                    if (c == '\n') {
                        break;
                    }
                }
                continue;
            }
            // Parse C89-style block comments.
            // Braces inside block comments don't contribute to brace matching.
            if (c == '/' && expect('*')) {
                while (true) {
                    if (eof()) {
                        return Rule_Error { Parse_Error_Code::unterminated_comment,
                                            Grammar_Rule::raw_content };
                    }
                    char c = pop();
                    if (c == '*' && expect('/')) {
                        break;
                    }
                }
                continue;
            }
        }

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

        while (true) {
            if (eof()) {
                break;
            }
            // Parse C99-style line comments.
            if (expect("//")) {
                newline_count = 0;
                while (true) {
                    if (eof()) {
                        return Parse_Error_Code::unterminated_comment;
                    }
                    const char c = pop();
                    if (c == '\n') {
                        newline_count = 1;
                        break;
                    }
                }
                continue;
            }
            // Parse C89-style block comments.
            if (expect("/*")) {
                newline_count = 0;
                while (true) {
                    if (eof()) {
                        return Parse_Error_Code::unterminated_comment;
                    }
                    char c = pop();
                    if (c == '*' && expect('/')) {
                        break;
                    }
                }
                continue;
            }
            // Any other non-comment, non-space character ends the blank.
            if (!peek(is_space)) {
                break;
            }
            // Last but not least, we consume whitespace.
            if (pop() == '\n') {
                is_paragraph_break |= ++newline_count > 1;
            }
        }

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
