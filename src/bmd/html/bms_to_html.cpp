#include "common/code_span_type.hpp"

#include "bms/diagnostic_consumer.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/token_type.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "bmd/html/bms_to_html.hpp"
#include "bmd/html/code_string_to_html.hpp"
#include "bmd/html/html_writer.hpp"

namespace bit_manipulation::bmd {

namespace {

[[nodiscard]] constexpr Code_Span_Type token_type_code_span_type(bms::Token_Type type)
{
    using enum bms::Token_Type;
    BIT_MANIPULATION_ASSERT(type != eof);

    switch (type) {
    case identifier: return Code_Span_Type::identifier;

    case left_parenthesis:
    case right_parenthesis:
    case left_brace:
    case right_brace: return Code_Span_Type::bracket;

    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: return Code_Span_Type::number;

    case string_literal: return Code_Span_Type::string;

    case block_comment:
    case line_comment: return Code_Span_Type::comment;

    case assign:
    case equals:
    case not_equals:
    case plus:
    case minus:
    case multiplication:
    case division:
    case remainder:
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal:
    case shift_left:
    case shift_right:
    case bitwise_and:
    case bitwise_or:
    case bitwise_not:
    case bitwise_xor:
    case logical_and:
    case logical_or:
    case logical_not: return Code_Span_Type::operation;

    case double_right_arrow: return Code_Span_Type::error;

    case at: return Code_Span_Type::annotation_name;

    case dot:
    case colon:
    case comma:
    case semicolon:
    case right_arrow: return Code_Span_Type::punctuation;

    case keyword_as:
    case keyword_let:
    case keyword_const:
    case keyword_function:
    case keyword_while:
    case keyword_if:
    case keyword_else:
    case keyword_requires:
    case keyword_return:
    case keyword_break:
    case keyword_continue:
    case keyword_static_assert: return Code_Span_Type::keyword;

    case keyword_true:
    case keyword_false: return Code_Span_Type::boolean_literal;

    case keyword_uint:
    case keyword_int:
    case keyword_bool:
    case keyword_void:
    case keyword_nothing: return Code_Span_Type::type_name;

    case eof: break;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid token type.");
}

/// @brief Categorizes a token at the index, taking surrounding tokens into account to e.g.
/// classify the kind of identifier.
///
/// For example, if `tokens[i]` is an `identifier` and `tokens[i - 1]` is `keyword_let`,
/// `Code_Span_Type::variable_name` is returned.
struct Token_Categorizer {
private: /// @brief the (non-empty) span of tokens
    std::span<const bms::Token> m_tokens;
    /// @brief the index within the span
    Size m_index = 0;
    Code_Span_Type m_identifier_bias = Code_Span_Type::variable_name;

public:
    [[nodiscard]] explicit Token_Categorizer(std::span<const bms::Token> tokens)
        : m_tokens { tokens }
    {
    }

    [[nodiscard]] Size index() const noexcept
    {
        return m_index;
    }

    void operator++()
    {
        BIT_MANIPULATION_ASSERT(m_index < m_tokens.size());
        ++m_index;
    }

    [[nodiscard]] Code_Span_Type operator*()
    {
        BIT_MANIPULATION_ASSERT(m_index < m_tokens.size());
        using enum bms::Token_Type;

        if (m_tokens[m_index].type != identifier) {
            return token_type_code_span_type(m_tokens[m_index].type);
        }
        if (m_index != 0) {
            switch (m_tokens[m_index - 1].type) {
            case keyword_let:
            case keyword_const:
                m_identifier_bias = Code_Span_Type::variable_name;
                return Code_Span_Type::variable_name;
            case keyword_function: //
                m_identifier_bias = Code_Span_Type::variable_name;
                return Code_Span_Type::function_name;
            case keyword_as:
            case colon:
            case right_arrow: return Code_Span_Type::type_name;
            case at: //
                m_identifier_bias = Code_Span_Type::identifier;
                return Code_Span_Type::annotation_name;
            case semicolon: //
                m_identifier_bias = Code_Span_Type::variable_name;
                break;
            default: break;
            }
        }
        if (m_index + 1 < m_tokens.size()) {
            switch (m_tokens[m_index + 1].type) {
            case assign: {
                // We can't say that identifiers preceding '=' are variables in general;
                // they could also be attribute arguments.
                if (m_index == 0 || m_tokens[m_index - 1].type == semicolon
                    || m_tokens[m_index - 1].type == identifier) {
                    return Code_Span_Type::variable_name;
                }
                break;
            }
            case colon:
            case keyword_as: return Code_Span_Type::variable_name;
            case left_parenthesis: return Code_Span_Type::function_name;
            default: break;
            }
        }
        return m_identifier_bias;
    }
};

void tokens_to_html(HTML_Writer& out, std::span<const bms::Token> tokens, std::string_view code)
{
    Token_Categorizer categorizer { tokens };

    for (Size i = 0; i < tokens.size(); ++i, ++categorizer) {
        BIT_MANIPULATION_ASSERT(categorizer.index() == i);

        // Originally, we were using write_source_gap here, which would be fine if we only had
        // successfully matched tokens, separated by whitespace.
        // However, things like illegal characters can also fill the gaps between tokens,
        // now that we use recoverable tokenization.
        // Therefore, we need to write the code in the gap literally.
        const Size previous_end = i == 0 ? 0 : tokens[i - 1].pos.end();
        const std::string_view gap = code.substr(previous_end, tokens[i].pos.begin - previous_end);
        out.write_inner_text(gap);

        const Code_Span_Type category = *categorizer;
        const std::string_view tag = code_span_type_tag(category);
        out.open_tag(tag);
        const std::string_view text = code.substr(tokens[i].pos.begin, tokens[i].pos.length);
        out.write_inner_text(text);
        out.close_tag(tag);
    }
    const std::string_view trailing_code
        = tokens.empty() ? code : code.substr(tokens.back().pos.end());
    out.write_inner_text(trailing_code);
}

} // namespace

bool bms_inline_code_to_html(HTML_Writer& out,
                             std::string_view code,
                             std::pmr::memory_resource* memory,
                             Function_Ref<void(bms::Tokenize_Error&&)> on_error)
{
    BIT_MANIPULATION_ASSERT(memory != nullptr);

    std::pmr::vector<bms::Token> tokens { memory };
    auto error_consumer = [&](bms::Tokenize_Error&& e) { //
        if (on_error) {
            on_error(std::move(e));
        }
        return bms::Error_Reaction::keep_going;
    };
    bool tokenize_succeeded = bms::tokenize(tokens, code, error_consumer);

    tokens_to_html(out, tokens, code);

    return tokenize_succeeded;
}

bool bms_inline_code_to_html(Code_String& out,
                             std::string_view code,
                             std::pmr::memory_resource* memory,
                             Function_Ref<void(bms::Tokenize_Error&&)> on_error)
{
    HTML_Writer writer { out };
    return bms_inline_code_to_html(writer, code, memory, on_error);
}

} // namespace bit_manipulation::bmd
