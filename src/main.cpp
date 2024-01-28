#include <iostream>
#include <stdexcept>

#include "assert.hpp"
#include "bmscript.hpp"
#include "io.hpp"

namespace bit_manipulation {
namespace {

struct Tokenized_File {
    std::vector<Token> tokens;
    std::string program;
};

Tokenized_File tokenize_file(std::string_view file)
{
    std::string program = file_to_string(file);

    std::vector<Token> tokens;
    if (const auto result = tokenize(tokens, program)) {
        return { std::move(tokens), std::move(program) };
    }
    else {
        throw std::runtime_error("Failed to tokenize");
    }
}

void print_tokens(const Tokenized_File& file)
{
    for (const Token& t : file.tokens) {
        const std::string_view text = t.extract(file.program);
        std::cout << t.pos.line + 1 << ":" << t.pos.column + 1 << ": " << token_type_name(t.type)
                  << "(" << text << ")";
        if (text.length() > 1) {
            std::cout << " (" << text.length() << " characters)";
        }
        std::cout << '\n';
    }
}

void dump_tokens(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    print_tokens(f);
}

void dump_ast(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
}

int main(int argc, const char** argv)
try {
    const std::vector<std::string_view> args(argv, argv + argc);

    if (args.size() < 3) {
        const std::string_view program_name = args.size() == 0 ? "bitmanip" : args[0];
        std::cout << "Usage: " << program_name << " dump_tokens|dump_ast <FILE>";
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