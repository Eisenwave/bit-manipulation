#include <fstream>
#include <iomanip>
#include <iostream>
#include <span>
#include <stdexcept>
#include <string_view>

#include "common/ansi.hpp"
#include "common/assert.hpp"
#include "common/diagnostics.hpp"
#include "common/glue.hpp"
#include "common/io.hpp"

#include "bmd/doc_to_html.hpp"
#include "bmd/html_writer.hpp"
#include "bmd/parse.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

namespace bit_manipulation {
namespace {

std::pmr::vector<bms::Token>
tokenize_bms_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::vector<bms::Token> tokens(memory);
    if (const Result<void, bms::Tokenize_Error> result = tokenize(tokens, source)) {
        return tokens;
    }
    else {
        print_tokenize_error(std::cout, file, source, result.error());
        std::exit(1);
    }
}

std::pmr::string load_file(std::string_view file, std::pmr::memory_resource* memory)
{
    Result<std::pmr::string, IO_Error_Code> result = file_to_string(file, memory);
    if (!result) {
        print_io_error(std::cout, file, result.error());
        std::exit(1);
    }
    return std::move(*result);
}

bmd::Parsed_Document
parse_bmd_file(std::string_view source, std::string_view file, std::pmr::memory_resource* memory)
{

    Result<bmd::Parsed_Document, bmd::Parse_Error> parsed = bmd::parse(source, memory);
    if (!parsed) {
        print_parse_error(std::cout, file, source, parsed.error());
        std::exit(1);
    }

    return std::move(*parsed);
}

bms::Parsed_Program parse_tokenized(std::span<bms::Token const> tokens,
                                    std::string_view source,
                                    std::string_view file_name,
                                    std::pmr::memory_resource* memory)
{
    if (Result<bms::Parsed_Program, bms::Parse_Error> parsed = bms::parse(tokens, source, memory)) {
        return std::move(*parsed);
    }
    else {
        print_parse_error(std::cout, file_name, source, parsed.error());
        std::exit(1);
    }
}

int dump_tokens(std::string_view file, std::pmr::memory_resource* memory)
{
    if (!file.ends_with(".bms")) {
        std::cout << ansi::red << "Error: file must have '.bms' suffix\n";
        return 1;
    }
    const std::pmr::string source = load_file(file, memory);
    const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, memory);
    print_tokens(std::cout, tokens, source);
    return 0;
}

int dump_ast(std::string_view file, std::pmr::memory_resource* memory)
{
    const std::pmr::string source = load_file(file, memory);

    constexpr Size indent_width = 2;
    if (file.ends_with(".bms")) {
        const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, memory);
        const bms::Parsed_Program p = parse_tokenized(tokens, source, file, memory);
        print_ast(std::cout, p, indent_width);
        return 0;
    }
    if (file.ends_with(".bmd")) {
        const bmd::Parsed_Document program = parse_bmd_file(source, file, memory);
        print_ast(std::cout, program, indent_width);
        return 0;
    }

    std::cout << ansi::red << "Error: unrecognized file suffix for: " << file << '\n';
    return 1;
}

int to_html(std::string_view file,
            std::optional<std::string_view> out_file,
            std::pmr::memory_resource* memory)
{
    const std::pmr::string source = load_file(file, memory);

    if (file.ends_with(".bms")) {
        std::cout << "Converting BMS files to HTML is not supported yet\n";
        return 0;
    }
    if (file.ends_with(".bmd")) {
        const bmd::Parsed_Document program = parse_bmd_file(source, file, memory);

        Result<void, bmd::Document_Error> result;

        if (!out_file) {
            if (!std::cout) {
                print_io_error(std::cerr, "stdout", IO_Error_Code::cannot_open);
                return 1;
            }
            Colored_HTML_Consumer consumer { std::cout };
            result = write_html(consumer, program, memory);
        }
        else {
            std::ofstream out { std::string(*out_file) };
            if (!out) {
                print_io_error(out, *out_file, IO_Error_Code::cannot_open);
                return 1;
            }

            Simple_HTML_Consumer consumer { out };
            result = write_html(consumer, program, memory);
        }

        if (!result) {
            print_document_error(std::cout, file, source, result.error());
            return 1;
        }

        return 0;
    }

    std::cout << ansi::red << "Error: unrecognized file suffix for: " << file << '\n';
    return 1;
}

int check_semantics(std::string_view file, std::pmr::memory_resource* memory)
{
    if (!file.ends_with(".bms")) {
        std::cout << ansi::red << "Error: file must have '.bms' suffix\n";
        return 1;
    }
    std::pmr::unsynchronized_pool_resource memory_resource(memory);
    const std::pmr::string source = load_file(file, &memory_resource);

    const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, &memory_resource);
    bms::Parsed_Program p = parse_tokenized(tokens, source, file, &memory_resource);
    bms::Analyzed_Program a(p, file, &memory_resource);

    Result<void, bms::Analysis_Error> result = bms::analyze(a, &memory_resource);
    if (!result) {
        print_analysis_error(std::cout, p, result.error());
        return 1;
    }

    std::cout << ansi::green << "All checks passed.\n" << ansi::reset;
    return 0;
}

int main(int argc, const char** argv)
try {
    const std::vector<std::string_view> args(argv, argv + argc);

    if (args.size() < 3) {
        const std::string_view program_name = args.size() == 0 ? "bitmanip" : args[0];
        std::cout << "Usage: " << program_name
                  << " dump_tokens|dump_ast|verify|html <FILE> [FILE]\n";
        return 1;
    }

    std::pmr::unsynchronized_pool_resource memory;

    if (args[1] == "dump_tokens") {
        return dump_tokens(args[2], &memory);
    }
    else if (args[1] == "dump_ast") {
        return dump_ast(args[2], &memory);
    }
    else if (args[1] == "verify") {
        return check_semantics(args[2], &memory);
    }
    else if (args[1] == "html") {
        return to_html(args[2], argc > 3 ? args[3] : std::optional<std::string_view> {}, &memory);
    }
    else {
        std::cout << "Unknown command '" << args[1] << "'\n";
        return 1;
    }
} catch (const Assertion_Error& e) {
    print_assertion_error(std::cout, e);
    return 1;
} catch (std::exception& e) {
    std::cout << ansi::h_red << "Unhandled exception! " << ansi::reset
              << "An exception with the following message has been raised:\n\n";
    std::cout << e.what() << "\n\n";
    print_internal_error_notice(std::cout);
    return 1;
} catch (...) {
    std::cout << ansi::h_red << "Unhandled exception! " << ansi::reset
              << "An exception not derived from std::exception has been raised.\n\n";
    print_internal_error_notice(std::cout);
    return 1;
}

} // namespace
} // namespace bit_manipulation

int main(int argc, const char** argv)
{
    return bit_manipulation::main(argc, argv);
}
