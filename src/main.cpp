#include <iomanip>
#include <iostream>
#include <stdexcept>

#include "ansi.hpp"
#include "assert.hpp"
#include "diagnostics.hpp"
#include "io.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

namespace bit_manipulation {
namespace {

struct Tokenized_File {
    std::pmr::vector<bms::Token> tokens;
    std::pmr::string program;
};

Tokenized_File tokenize_file(std::string_view file, std::pmr::memory_resource* memory)
{
    Result<std::pmr::string, IO_Error_Code> program = file_to_string(file, memory);
    if (!program) {
        print_location_of_file(std::cout, file) << ": " << to_prose(program.error()) << '\n';
        std::exit(1);
    }

    std::pmr::vector<bms::Token> tokens(memory);
    if (const Result<void, bms::Tokenize_Error> result = tokenize(tokens, *program)) {
        return { std::move(tokens), std::move(*program) };
    }
    else {
        print_tokenize_error(std::cout, file, *program, result.error());
        std::exit(1);
    }
}

bms::Parsed_Program parse_tokenized(std::string_view file_name,
                                    const Tokenized_File& f,
                                    std::pmr::memory_resource* memory)
{
    if (Result<bms::Parsed_Program, bms::Parse_Error> parsed = parse(f.tokens, f.program, memory)) {
        return std::move(*parsed);
    }
    else {
        print_parse_error(std::cout, file_name, f.program, parsed.error());
        std::exit(1);
    }
}

int dump_tokens(std::string_view file, std::pmr::memory_resource* memory)
{
    const Tokenized_File f = tokenize_file(file, memory);
    print_tokens(std::cout, f.tokens, f.program);
    return 0;
}

int dump_ast(std::string_view file, std::pmr::memory_resource* memory)
{
    constexpr Size indent_width = 2;

    const Tokenized_File f = tokenize_file(file, memory);
    const bms::Parsed_Program p = parse_tokenized(file, f, memory);
    print_ast(std::cout, p, indent_width);
    return 0;
}

int check_semantics(std::string_view file, std::pmr::memory_resource* memory)
{
    std::pmr::unsynchronized_pool_resource memory_resource(memory);

    const Tokenized_File f = tokenize_file(file, &memory_resource);
    bms::Parsed_Program p = parse_tokenized(file, f, &memory_resource);
    bms::Analyzed_Program a(p, &memory_resource);

    Result<void, bms::Analysis_Error> result = bms::analyze(a, &memory_resource);
    if (result) {
        std::cout << ansi::green << "All checks passed.\n" << ansi::reset;
        return 0;
    }
    else {
        print_analysis_error(std::cout, file, p, result.error());
        return 1;
    }
}

int main(int argc, const char** argv)
try {
    const std::vector<std::string_view> args(argv, argv + argc);

    if (args.size() < 3) {
        const std::string_view program_name = args.size() == 0 ? "bitmanip" : args[0];
        std::cout << "Usage: " << program_name << " dump_tokens|dump_ast|verify <FILE>\n";
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
    else {
        std::cout << "Unknown command '" << args[1] << "'\n";
        return 1;
    }
} catch (std::exception& e) {
    std::cout
        << ansi::h_red << "Assertion failed! " << ansi::reset
        << "The following expression evaluated to 'false', but was expected to be 'true':\n\n";
    std::cout << e.what() << "\n\n";
    print_internal_error_notice(std::cout);
    return 1;
}

} // namespace
} // namespace bit_manipulation

int main(int argc, const char** argv)
{
    return bit_manipulation::main(argc, argv);
}