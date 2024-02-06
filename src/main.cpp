#include <iomanip>
#include <iostream>
#include <stdexcept>

#include "ansi.hpp"
#include "assert.hpp"
#include "diagnostics.hpp"
#include "io.hpp"

#include "bms/analysis_error.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"

namespace bit_manipulation {
namespace {

struct Tokenized_File {
    std::vector<bms::Token> tokens;
    std::string program;
};

Tokenized_File tokenize_file(std::string_view file)
{
    Result<std::string, IO_Error_Code> program = file_to_string(file);
    if (!program) {
        print_file_location(std::cout, file) << ": " << to_prose(program.error()) << '\n';
        std::exit(1);
    }

    std::vector<bms::Token> tokens;
    if (const Result<void, bms::Tokenize_Error> result = tokenize(tokens, *program)) {
        return { std::move(tokens), std::move(*program) };
    }
    else {
        print_tokenize_error(std::cout, file, *program, result.error());
        std::exit(1);
    }
}

void print_ast(std::ostream& out, const bms::Parsed_Program& program)
{
    (void)out;
    (void)program;
}

void dump_tokens(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    print_tokens(std::cout, f.tokens, f.program);
}

void dump_ast(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    if (Result<bms::Parsed_Program, bms::Parse_Error> parsed = parse(f.tokens, f.program)) {
        return print_ast(std::cout, *parsed);
    }
    else {
        print_parse_error(std::cout, file, f.program, parsed.error());
        std::exit(1);
    }
}

int main(int argc, const char** argv)
try {
    const std::vector<std::string_view> args(argv, argv + argc);

    if (args.size() < 3) {
        const std::string_view program_name = args.size() == 0 ? "bitmanip" : args[0];
        std::cout << "Usage: " << program_name << " dump_tokens|dump_ast <FILE>\n";
        return 1;
    }

    if (args[1] == "dump_tokens") {
        dump_tokens(args[2]);
        return 0;
    }

    if (args[1] == "dump_ast") {
        dump_ast(args[2]);
    }

    return 0;
} catch (std::exception& e) {
    std::cout << "Error: " << e.what() << '\n';
    return 1;
}

} // namespace
} // namespace bit_manipulation

int main(int argc, const char** argv)
{
    return bit_manipulation::main(argc, argv);
}