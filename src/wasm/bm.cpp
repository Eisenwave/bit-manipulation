#include <charconv>
#include <cstring>
#include <memory_resource>
#include <string>

#include "common/assert.hpp"
#include "common/code_string.hpp"
#include "common/config.hpp"
#include "common/diagnostics.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/diagnostic_consumer.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/tokenize.hpp"
#include "bms/vm/codegen.hpp"

#include "bmd/code_language.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/html/bms_to_html.hpp"
#include "bmd/html/code_string_to_html.hpp"
#include "bmd/html/html_writer.hpp"
#include "bmd/html/token_consumer.hpp"

#include "wasm/bm.hpp"

namespace bit_manipulation {
namespace {

constexpr std::string_view file_name = "input";

bm_allocation copy_to_heap(const void* data, Uint32 n)
{
    void* memory = bm_foreign_alloc(n);
    if (memory != nullptr) {
        std::memcpy(memory, data, n);
    }
    return { memory, n };
}

bm_allocation copy_to_heap(std::string_view str)
{
    return copy_to_heap(str.data(), Uint32(str.size()));
}

/// @brief An HTML token consumer which ignores token types and dumps all
/// character data into a `vector`.
struct Buffered_HTML_Token_Consumer final : bmd::Data_HTML_Token_Consumer {
    std::pmr::vector<char> out;

    [[nodiscard]] explicit Buffered_HTML_Token_Consumer(std::pmr::memory_resource* memory)
        : out(memory)
    {
    }

    bool write(char c) final
    {
        out.push_back(c);
        return true;
    }

    bool write(char c, Size n) final
    {
        out.insert(out.end(), n, c);
        return true;
    }

    bool write(std::string_view s) final
    {
        out.insert(out.end(), s.begin(), s.end());
        return true;
    }

    [[nodiscard]] std::string_view as_string() const
    {
        return { out.data(), out.size() };
    }
};

bm_allocation html_to_heap(const Code_String& string, std::pmr::memory_resource* memory)
{
    // Everything is pre-formatted in our use case, so this number doesn't really matter.
    constexpr Size indent_width = 0;
    Buffered_HTML_Token_Consumer consumer { memory };
    bmd::HTML_Writer writer { consumer, indent_width };
    bmd::code_string_to_html(writer, string);

    return copy_to_heap(consumer.out.data(), consumer.out.size());
}

bm_text_result html_to_heap_as_result(const Code_String& string, std::pmr::memory_resource* memory)
{
    return { .allocation = html_to_heap(string, memory), .is_html = true };
}

bm_text_result
to_heap_raw_or_html(const Code_String& string, std::pmr::memory_resource* memory, bool as_html)
{
    if (!as_html) {
        return { .allocation = copy_to_heap(string.get_text()), .is_html = false };
    }
    return html_to_heap_as_result(string, memory);
}

bm_text_result error_to_heap(const bms::Tokenize_Error& error,
                             std::string_view source,
                             std::pmr::memory_resource* memory,
                             bool as_html)
{
    Code_String error_out { memory };
    print_tokenize_error(error_out, file_name, source, error);
    return to_heap_raw_or_html(error_out, memory, as_html);
}

bm_text_result error_to_heap(const bms::Parse_Error& error,
                             std::string_view source,
                             std::pmr::memory_resource* memory,
                             bool as_html)
{
    Code_String error_out { memory };
    print_parse_error(error_out, file_name, source, error);
    return to_heap_raw_or_html(error_out, memory, as_html);
}

bm_text_result error_to_heap(const bms::Analysis_Error& error,
                             const bms::Parsed_Program& parsed,
                             std::pmr::memory_resource* memory,
                             bool as_html)
{
    Code_String error_out { memory };
    print_analysis_error(error_out, parsed, error);
    return to_heap_raw_or_html(error_out, memory, as_html);
}

bm_text_result
error_to_heap(const bmd::Generator_Error& error, std::pmr::memory_resource* memory, bool as_html)
{
    Code_String error_out { memory };
    print_generator_error(error_out, error);
    return to_heap_raw_or_html(error_out, memory, as_html);
}

bm_text_result translate_to(std::string_view source,
                            bmd::Code_Language lang,
                            bmd::Code_Options options,
                            bool as_html)
{
    std::pmr::unsynchronized_pool_resource memory;

    std::pmr::vector<bms::Token> tokens { &memory };

    if (Result<void, bms::Tokenize_Error> r = bms::tokenize(tokens, source); !r) {
        return error_to_heap(r.error(), source, &memory, as_html);
    }

    bms::Parsed_Program parsed { source, &memory };
    if (Result<void, bms::Parse_Error> r = bms::parse(parsed, tokens); !r) {
        return error_to_heap(r.error(), source, &memory, as_html);
    }

    bms::Analyzed_Program analyzed { parsed, file_name, &memory };
    if (Result<void, bms::Analysis_Error> r = bms::analyze(analyzed, parsed, &memory); !r) {
        return error_to_heap(r.error(), parsed, &memory, as_html);
    }

    if (lang == bmd::Code_Language::bms_vm) {
        bms::generate_code(analyzed, { .calls = bms::Call_Policy::always_symbolic });
    }

    Code_String out { &memory };
    if (Result<void, bmd::Generator_Error> r
        = bmd::generate_code(out, analyzed, lang, &memory, options);
        !r) {
        return error_to_heap(r.error(), &memory, as_html);
    }
    return to_heap_raw_or_html(out, &memory, as_html);
}

bm_allocation syntax_highlight(std::string_view source)
{
    std::pmr::unsynchronized_pool_resource memory;
    Buffered_HTML_Token_Consumer out { &memory };

    bmd::bms_inline_code_to_html(out, source, &memory);
    return copy_to_heap(out.as_string());
}

[[nodiscard]] bmd::Code_Language preset_code_language(Codegen_Preset preset)
{
    using enum Codegen_Preset;
    switch (preset) {
    case bms: return bmd::Code_Language::bms;
    case bms_vm: return bmd::Code_Language::bms_vm;
    case c99:
    case c23: return bmd::Code_Language::c;
    case cpp: return bmd::Code_Language::cpp;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid preset.");
}

[[nodiscard]] bmd::Code_Options preset_code_options(Codegen_Preset preset)
{
    using enum Codegen_Preset;
    switch (preset) {
    case c23: return { .c_23 = true };
    default: return {};
    }
}

} // namespace
} // namespace bit_manipulation

using namespace bit_manipulation;

extern "C" {

static std::pmr::unsynchronized_pool_resource foreign_allocations_resource;

[[nodiscard]] void* bm_foreign_alloc(Uint32 n)
{
    return foreign_allocations_resource.allocate(n);
}

void bm_foreign_free(void* p, Uint32 n)
{
    return foreign_allocations_resource.deallocate(p, n);
}

int bm_plus(int x, int y)
{
    return x + y;
}

bm_text_result bm_length_as_string_result;

void bm_length_as_string(const char* str)
{
    char buffer[32];
    auto result = std::to_chars(buffer, std::end(buffer), std::strlen(str));
    BIT_MANIPULATION_ASSERT(result.ec == std::error_code {});

    bm_length_as_string_result = { copy_to_heap(buffer, Uint32(result.ptr - buffer)), false };
}

bm_text_result bm_translate_code_result {};

void bm_translate_code(const char* source_data, Uint32 length, Codegen_Preset preset)
{
    constexpr bool as_html = true;

    const std::string_view source = { source_data, length };
    const auto lang = preset_code_language(preset);
    const auto options = preset_code_options(preset);

    bm_translate_code_result = bit_manipulation::translate_to(source, lang, options, as_html);
}

bm_allocation bm_syntax_highlight_result;

void bm_syntax_highlight(const char* source, Uint32 source_length)
{
    bm_syntax_highlight_result = syntax_highlight({ source, source_length });
}

//
}
