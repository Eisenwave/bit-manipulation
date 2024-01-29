#include <iomanip>
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

void print_tokens(std::ostream& out, const Tokenized_File& file)
{
    for (const Token& t : file.tokens) {
        const std::string_view text = t.extract(file.program);
        out << std::setfill(' ') << std::right //
            << std::setw(2) << t.pos.line + 1 //
            << ":" //
            << std::setw(2) << t.pos.column + 1 << ": " //
            << std::left //
            << token_type_name(t.type);

        if (token_type_length(t.type) == 0) {
            out << "(" << text << ")";
            if (text.length() > 1) {
                out << " (" << text.length() << " characters)";
            }
        }

        out << '\n';
    }
}

void print_ast(std::ostream& out, const ast::Node& root)
{
    (void)out;
    (void)root;
}

void dump_tokens(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    print_tokens(std::cout, f);
}

void dump_ast(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    Parse_Result parsed = parse(f.tokens, f.program);

    if (const Parse_Error* node = std::get_if<Parse_Error>(&parsed)) {
        std::cout << file << ":" << node->fail_token.pos.line + 1 << ":"
                  << node->fail_token.pos.column << ": error: ";
        std::cout << "expected '" << grammar_rule_name(node->fail_rule) << "', but found token '"
                  << token_type_name(node->fail_token.type) << "'\n";
        return;
    }

    if (const ast::Node* node = std::get_if<ast::Node>(&parsed)) {
        print_ast(std::cout, *node);
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