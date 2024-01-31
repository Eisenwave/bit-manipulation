#include <iomanip>
#include <iostream>
#include <stdexcept>

#include "ansi.hpp"
#include "assert.hpp"
#include "io.hpp"

#include "bms/bms.hpp"

namespace bit_manipulation {
namespace {

struct Tokenized_File {
    std::vector<bms::Token> tokens;
    std::string program;
};

Tokenized_File tokenize_file(std::string_view file)
{
    std::string program = file_to_string(file);

    std::vector<bms::Token> tokens;
    if (const auto result = tokenize(tokens, program)) {
        return { std::move(tokens), std::move(program) };
    }
    else {
        throw std::runtime_error("Failed to tokenize");
    }
}

void print_tokens(std::ostream& out, const Tokenized_File& file)
{
    for (const bms::Token& t : file.tokens) {
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

void print_ast(std::ostream& out, const bms::Parsed_Program& program)
{
    (void)out;
    (void)program;
}

void dump_tokens(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    print_tokens(std::cout, f);
}

void print_file_position(std::string_view file, bms::Source_Position pos)
{
    std::cout << ansi::black << file << ":" << pos.line + 1 << ":" << pos.column << ansi::reset;
}

std::string_view find_line(std::string_view source, Size index)
{
    Size begin = source.rfind('\n', index);
    begin = begin != std::string_view::npos ? begin + 1 : 0;

    Size end = std::min(source.find('\n', index + 1), source.size());

    return source.substr(begin, end - begin);
}

void print_affected_line(std::string_view source, bms::Source_Position pos)
{
    const std::string_view line = find_line(source, pos.begin);
    std::cout << std::right << std::setfill(' ') //
              << ansi::h_yellow << std::setw(5) << pos.line + 1 << ansi::reset << " |" //
              << line << '\n' //
              << std::setw(5) << ""
              << " |" //
              << std::string(pos.column, ' ') << ansi::h_green << "^\n"
              << ansi::reset;
}

void dump_ast(std::string_view file)
{
    const Tokenized_File f = tokenize_file(file);
    bms::Parse_Result parsed = parse(f.tokens, f.program);

    if (const bms::Parse_Error* node = std::get_if<bms::Parse_Error>(&parsed)) {
        print_file_position(file, node->fail_token.pos);
        std::cout << ": " << ansi::h_red << "error: " << ansi::reset //
                  << "while matching '" << grammar_rule_name(node->fail_rule)
                  << "', unexpected token " << token_type_readable_name(node->fail_token.type)
                  << "\n";
        print_file_position(file, node->fail_token.pos);
        std::cout << ": note: expected ";

        const std::span<const bms::Token_Type> expected = node->expected_tokens;
        if (expected.size() == 0) {
            std::cout << "nothing";
        }
        else if (expected.size() == 1) {
            std::cout << token_type_readable_name(expected[0]);
        }
        else {
            std::cout << "one of: ";
            for (Size i = 0; i < expected.size(); ++i) {
                std::cout << (i + 1 == expected.size() ? ", or " : i != 0 ? ", " : "");
                std::cout << token_type_readable_name(expected[i]);
            }
        }
        std::cout << "\n";
        print_affected_line(f.program, node->fail_token.pos);
    }

    if (const auto* program = std::get_if<bms::Parsed_Program>(&parsed)) {
        print_ast(std::cout, *program);
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