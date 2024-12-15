#include <iostream>

#include "common/diagnostics.hpp"

#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "bmd/codegen/code_string.hpp"

#include "cli/compile.hpp"
#include "cli/glue.hpp"

namespace bit_manipulation {

// FIXME: detect TTY
constexpr bool colors = true;

std::pmr::vector<bms::Token>
tokenize_bms_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::vector<bms::Token> tokens(memory);
    if (const Result<void, bms::Tokenize_Error> result = tokenize(tokens, source)) {
        return tokens;
    }
    else {
        bmd::Code_String out { memory };
        print_tokenize_error(out, file, source, result.error());
        print_code_string(std::cout, out, colors);
        std::exit(1);
    }
}

std::pmr::string load_file(std::string_view file, std::pmr::memory_resource* memory)
{
    Result<std::pmr::string, IO_Error_Code> result = file_to_string(file, memory);
    if (!result) {
        bmd::Code_String out { memory };
        print_io_error(out, file, result.error());
        print_code_string(std::cout, out, colors);
        std::exit(1);
    }
    return std::move(*result);
}

bmd::Parsed_Document
parse_bmd_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{

    Result<bmd::Parsed_Document, bmd::Parse_Error> parsed = bmd::parse(source, memory);
    if (!parsed) {
        bmd::Code_String out { memory };
        print_parse_error(out, file, source, parsed.error());
        print_code_string(std::cout, out, colors);
        std::exit(1);
    }

    return std::move(*parsed);
}

bms::Parsed_Program parse_tokenized(std::span<bms::Token const> tokens,
                                    std::string_view source,
                                    std::string_view file_name,
                                    std::pmr::memory_resource* memory)
{
    bms::Parsed_Program parsed { source, memory };
    if (Result<void, bms::Parse_Error> result = bms::parse(parsed, tokens)) {
        return parsed;
    }
    else {
        bmd::Code_String out { memory };
        print_parse_error(out, file_name, source, result.error());
        print_code_string(std::cout, out, colors);
        std::exit(1);
    }
}

bms::Analyzed_Program analyze_parsed(const bms::Parsed_Program& parsed,
                                     std::string_view file_name,
                                     std::pmr::memory_resource* memory)
{
    bms::Analyzed_Program analyzed { parsed, file_name, memory };

    if (Result<void, bms::Analysis_Error> result = bms::analyze(analyzed, parsed, memory)) {
        return analyzed;
    }
    else {
        bmd::Code_String out { memory };
        print_analysis_error(out, parsed, result.error());
        print_code_string(std::cout, out, colors);
        std::exit(1);
    }
}

} // namespace bit_manipulation
