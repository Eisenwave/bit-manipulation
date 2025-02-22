#include <algorithm>
#include <memory_resource>
#include <vector>

#include <gtest/gtest.h>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/io.hpp"
#include "common/tty.hpp"

#include "bms/tokenization/token.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "test/bms/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

const bool should_print_colors = is_tty(stdout);

struct Textual_Token {
    bms::Token_Type type;
    std::string_view text = bms::token_type_code_name(type);

    [[nodiscard]] constexpr bool empty() const
    {
        return text.empty();
    }

    [[maybe_unused]] [[nodiscard]] friend std::strong_ordering operator<=>(const Textual_Token&,
                                                                           const Textual_Token&)
        = default;
};

[[nodiscard]] constexpr Textual_Token extract_token(std::string_view source,
                                                    const bms::Token& token)
{
    return { .type = token.type, .text = { source.data() + token.pos.begin, token.pos.length } };
}

void print_token(Code_String& out, const Textual_Token& token)
{
    out.append(bms::token_type_name(token.type), Code_Span_Type::diagnostic_attribute);
    out.append('(', Code_Span_Type::bracket);
    out.append(token.text, Code_Span_Type::diagnostic_code_citation);
    out.append(')', Code_Span_Type::bracket);
}

struct Tokenized_Program {
    std::pmr::vector<char> source;
    std::pmr::vector<bms::Token> tokens;

    [[nodiscard]] std::string_view get_source_string() const
    {
        return { source.data(), source.size() };
    }

    [[nodiscard]] bool check_equals(std::span<const Textual_Token> expected) const
    {
        const auto source_string = get_source_string();

        Code_String error;
        if (tokens.size() != expected.size()) {
            error.append("Test failed because amount of tokens doesn't match. ",
                         Code_Span_Type::diagnostic_error_text);
            error.append("Expected ", Code_Span_Type::diagnostic_error_text);
            error.append_integer(expected.size(), Code_Span_Type::diagnostic_line_number);
            error.append(", but got ", Code_Span_Type::diagnostic_error_text);
            error.append_integer(tokens.size(), Code_Span_Type::diagnostic_line_number);
            error.append(" tokens:\n\n", Code_Span_Type::diagnostic_error_text);
            print_tokens(error, tokens, source_string);
            print_code_string(std::cout, error, should_print_colors);
            return false;
        }
        for (Size i = 0; i < tokens.size(); ++i) {
            const Textual_Token actual = extract_token(source_string, tokens[i]);
            const Textual_Token e = expected[i];
            if (actual != e) {
                error.append("Test failed because of token mismatch at index ",
                             Code_Span_Type::diagnostic_error_text);
                error.append_integer(i, Code_Span_Type::diagnostic_line_number);
                error.append(". ");
                error.append("Expected ", Code_Span_Type::diagnostic_error_text);
                print_token(error, e);
                error.append(", but got ", Code_Span_Type::diagnostic_error_text);
                print_token(error, actual);
                error.append('\n');
                print_code_string(std::cout, error, should_print_colors);
                return false;
            }
        }
        return true;
    }
};

std::optional<Tokenized_Program> tokenize(std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::string full_file { "test/", memory };
    full_file += file;

    Result<std::pmr::vector<char>, bit_manipulation::IO_Error_Code> source
        = file_to_bytes(full_file, memory);
    if (!source) {
        Code_String out { memory };
        print_io_error(out, full_file, source.error());
        print_code_string(std::cout, out, should_print_colors);
        return {};
    }
    const std::string_view source_string { source->data(), source->size() };

    std::pmr::vector<bms::Token> tokens { memory };
    if (Result<void, bms::Tokenize_Error> r = bms::tokenize(tokens, source_string); !r) {
        Code_String out { memory };
        print_tokenize_error(out, full_file, source_string, r.error());
        print_code_string(std::cout, out, should_print_colors);
        return {};
    }

    return Tokenized_Program { std::move(*source), std::move(tokens) };
}

TEST(Tokenize, token_length_consistent)
{
    static_assert(std::is_same_v<Default_Underlying, std::underlying_type_t<bms::Token_Type>>);

    constexpr auto limit = Default_Underlying(bms::Token_Type::keyword_static_assert);
    for (Default_Underlying i = 0; i < limit; ++i) {
        const auto type = bms::Token_Type(i);
        const Size string_length = bms::token_type_code_name(type).length();
        const Size simple_length = bms::token_type_length(type);
        EXPECT_EQ(string_length, simple_length);
    }
}

TEST(Tokenize, regression_1)
{
    using enum bms::Token_Type;

    static const Textual_Token expected[] {
        { keyword_return },    { left_parenthesis }, { logical_not },      { identifier, "x" },
        { right_parenthesis }, { logical_and },      { left_parenthesis }, { identifier, "y" },
        { logical_or },        { logical_not },      { identifier, "y" },  { right_parenthesis },
        { semicolon },
    };

    std::pmr::monotonic_buffer_resource memory;
    std::optional<Tokenized_Program> actual = tokenize("syntax/tokenize_regression.bms", &memory);

    ASSERT_TRUE(actual);
    ASSERT_TRUE(actual->check_equals(expected));
}

TEST(Tokenize, all_tokens)
{
    using enum bms::Token_Type;

    // clang-format off
    static const Textual_Token expected[] {
        { identifier, "identifier" },
        { left_parenthesis },
        { right_parenthesis },
        { decimal_literal, "12345" },
        { octal_literal, "0242" },
        { hexadecimal_literal, "0xff" },
        { binary_literal, "0b11" },
        { string_literal, "\"abc\"" },
        { left_brace },
        { right_brace },
        { block_comment, "/* ... */" },
        { line_comment, "// ..." },
        { assign },
        { equals },
        { not_equals },
        { plus },
        { minus },
        { multiplication },
        { division },
        { remainder },
        { less_than },
        { greater_than },
        { less_or_equal },
        { greater_or_equal },
        { shift_left },
        { shift_right },
        { bitwise_and },
        { bitwise_or },
        { bitwise_not },
        { bitwise_xor },
        { logical_and },
        { logical_or },
        { logical_not },
        { right_arrow },
        { double_right_arrow },
        { dot },
        { colon },
        { comma },
        { at },
        { semicolon },
        { keyword_as },
        { keyword_let },
        { keyword_const },
        { keyword_function },
        { keyword_while },
        { keyword_if },
        { keyword_else },
        { keyword_uint },
        { keyword_int },
        { keyword_bool },
        { keyword_void },
        { keyword_nothing },
        { keyword_requires },
        { keyword_return },
        { keyword_break },
        { keyword_continue },
        { keyword_true },
        { keyword_false },
        { keyword_static_assert },
     };
    // clang-format on

    // Note that eof is deliberately not in the array because it is the only type of token that is
    // never produced by the tokenizer.
    static_assert(std::size(expected) == Size(keyword_static_assert),
                  "Incomplete test expectations. Update the array above with any missing values.");
    // This assertion would fail only as the result of developer error.
    BIT_MANIPULATION_ASSERT(std::ranges::none_of(expected, &Textual_Token::empty));

    std::pmr::monotonic_buffer_resource memory;
    std::optional<Tokenized_Program> actual = tokenize("syntax/all_tokens.bms", &memory);

    ASSERT_TRUE(actual);
    ASSERT_TRUE(actual->check_equals(expected));
}

} // namespace
} // namespace bit_manipulation
