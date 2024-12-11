#include <charconv>
#include <cstring>
#include <memory_resource>
#include <sstream>
#include <string>

#include "common/assert.hpp"
#include "common/config.hpp"
#include "common/diagnostics.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "bmd/codegen/code_string.hpp"
#include "bmd/codegen/codegen.hpp"

#include "wasm/bm.hpp"

namespace bit_manipulation {
namespace {

constexpr std::string_view file_name = "input";

using pmr_ostringstream
    = std::basic_ostringstream<char, std::char_traits<char>, std::pmr::polymorphic_allocator<char>>;

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

bm_allocation to_heap(pmr_ostringstream&& stream)
{
    std::pmr::string result = std::move(stream).str();
    return copy_to_heap(result);
}

bm_allocation error_to_heap(const bms::Tokenize_Error& error,
                            std::string_view source,
                            std::pmr::memory_resource* memory)
{
    pmr_ostringstream error_out { std::ios::out, memory };
    print_tokenize_error(error_out, file_name, source, error, false);
    return to_heap(std::move(error_out));
}

bm_allocation error_to_heap(const bms::Parse_Error& error,
                            std::string_view source,
                            std::pmr::memory_resource* memory)
{
    pmr_ostringstream error_out { std::ios::out, memory };
    print_parse_error(error_out, file_name, source, error, false);
    return to_heap(std::move(error_out));
}

bm_allocation error_to_heap(const bms::Analysis_Error& error,
                            const bms::Parsed_Program& parsed,
                            std::pmr::memory_resource* memory)
{
    pmr_ostringstream error_out { std::ios::out, memory };
    print_analysis_error(error_out, parsed, error, false);
    return to_heap(std::move(error_out));
}

bm_allocation translate_to(std::string_view source, bmd::Code_Language lang)
{
    std::pmr::unsynchronized_pool_resource memory;

    std::pmr::vector<bms::Token> tokens { &memory };

    if (Result<void, bms::Tokenize_Error> r = bms::tokenize(tokens, source); !r) {
        return error_to_heap(r.error(), source, &memory);
    }

    bms::Parsed_Program parsed { source, &memory };
    if (Result<void, bms::Parse_Error> r = bms::parse(parsed, tokens); !r) {
        return error_to_heap(r.error(), source, &memory);
    }

    bms::Analyzed_Program analyzed { parsed, file_name, &memory };
    if (Result<void, bms::Analysis_Error> r = bms::analyze(analyzed, parsed, &memory); !r) {
        return error_to_heap(r.error(), parsed, &memory);
    }

    bmd::Code_String out { &memory };
    bmd::generate_code(out, analyzed, lang, { .c_23 = true });

    return copy_to_heap(out.get_text());
}

} // namespace
} // namespace bit_manipulation

using namespace bit_manipulation;

extern "C" {

static std::pmr::unsynchronized_pool_resource foreign_allocations_resource;

void* bm_foreign_alloc(Uint32 n)
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

bm_allocation bm_length_as_string_result;

void bm_length_as_string(const char* str)
{
    char buffer[32];
    auto result = std::to_chars(buffer, std::end(buffer), std::strlen(str));
    BIT_MANIPULATION_ASSERT(result.ec == std::error_code {});

    bm_length_as_string_result = copy_to_heap(buffer, Uint32(result.ptr - buffer));
}

bm_allocation bm_translate_code_result;

void bm_translate_code(const char* source, Uint32 length, Uint8 lang)
{
    bm_translate_code_result
        = bit_manipulation::translate_to({ source, length }, bmd::Code_Language { lang });
}

//
}
