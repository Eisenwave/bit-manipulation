#include <fstream>
#include <iomanip>
#include <iostream>
#include <span>
#include <stdexcept>
#include <string_view>

#include "common/ansi.hpp"
#include "common/assert.hpp"
#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/io.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/tokenization/tokenize.hpp"

#include "bmd/code_language.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/html/doc_to_html.hpp"
#include "bmd/html/document_error.hpp"
#include "bmd/parsing/parse.hpp"

#include "cli/compile.hpp"
#include "cli/glue.hpp"

namespace bit_manipulation {
namespace {

// FIXME: detect tty
constexpr bool colors = true;

int dump_tokens(std::string_view file, std::pmr::memory_resource* memory)
{
    if (bmd::code_language_by_file(file) != bmd::Code_Language::bms) {
        std::cout << ansi::red << "Error: file must have '.bms' suffix\n";
        return 1;
    }
    const std::pmr::vector<char> source_data = load_file(file, memory);
    const std::string_view source { source_data.data(), source_data.size() };
    const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, memory);
    Code_String out { memory };
    print_tokens(out, tokens, source);
    print_code_string(std::cout, out, colors);
    return 0;
}

int dump_ast(std::string_view file, std::pmr::memory_resource* memory)
{
    const std::pmr::vector<char> source_data = load_file(file, memory);
    const std::string_view source { source_data.data(), source_data.size() };

    switch (bmd::code_language_by_file(file)) {
    case bmd::Code_Language::bms: {
        const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, memory);
        const bms::Parsed_Program p = parse_tokenized(tokens, source, file, memory);
        Code_String out { memory };
        print_ast(out, p, { .indent_width = 2 });
        print_code_string(std::cout, out, colors);
        return 0;
    }

    case bmd::Code_Language::bmd: {
        const bmd::Parsed_Document program = bmd::parse(source, memory);
        Code_String out { memory };
        print_ast(out, program, { .indent_width = 2, .max_node_text_length = 30 });
        print_code_string(std::cout, out, colors);
        return 0;
    }

    default: {
        if (colors) {
            std::cout << ansi::red;
        }
        std::cout << "Error: expected '.bms' or '.bmd' file, but got: " << file << '\n';
        return 1;
    }
    }
}

int to_html(std::string_view file,
            std::optional<std::string_view> out_file,
            std::pmr::memory_resource* memory)
{
    const std::pmr::vector<char> source_data = load_file(file, memory);
    const std::string_view source { source_data.data(), source_data.size() };

    switch (bmd::code_language_by_file(file)) {
    case bmd::Code_Language::bms: {
        std::cout << "Converting BMS files to HTML is not supported yet\n";
        return 0;
    }

    case bmd::Code_Language::bmd: {
        const bmd::Parsed_Document program = bmd::parse(source, memory);

        Result<void, bmd::Document_Error> result;

        if (!out_file) {
            if (!std::cout) {
                Code_String out { memory };
                print_io_error(out, "stdout", IO_Error_Code::cannot_open);
                print_code_string(std::cerr, out, colors);
                return 1;
            }
// FIXME reimplement
#if 0
            Colored_HTML_Consumer consumer { std::cout };
            result = write_html(consumer, program, memory);
#endif
        }
        else {
            std::ofstream out { std::string(*out_file) };
            if (!out) {
                Code_String out { memory };
                print_io_error(out, *out_file, IO_Error_Code::cannot_open);
                print_code_string(std::cout, out, colors);
                return 1;
            }

// FIXME reimplement
#if 0
            Simple_HTML_Consumer consumer { out };
            result = write_html(consumer, program, memory);
#endif
        }

        if (!result) {
            Code_String out { memory };
            print_document_error(out, file, source, result.error());
            print_code_string(std::cout, out, colors);
            return 1;
        }

        return 0;
    }

    default: {
        if (colors) {
            std::cout << ansi::red;
        }
        std::cout << "Error: expected '.bms' or '.bmd' file, but got: " << file << '\n';
        return 1;
    }
    }
}

int check_semantics(std::string_view file, std::pmr::memory_resource* memory)
{
    if (bmd::code_language_by_file(file) != bmd::Code_Language::bms) {
        if (colors) {
            std::cout << ansi::red;
        }
        std::cout << "Error: file must have '.bms' suffix\n";
        return 1;
    }
    std::pmr::unsynchronized_pool_resource memory_resource(memory);
    const std::pmr::vector<char> source_data = load_file(file, &memory_resource);
    const std::string_view source { source_data.data(), source_data.size() };

    const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, &memory_resource);
    bms::Parsed_Program p = parse_tokenized(tokens, source, file, &memory_resource);
    bms::Analyzed_Program a = analyze_parsed(p, file, &memory_resource);

    if (colors) {
        std::cout << ansi::green;
    }
    std::cout << "All checks passed.\n";
    if (colors) {
        std::cout << ansi::reset;
    }
    return 0;
}

int generate(std::string_view file,
             std::string_view language_name,
             std::pmr::memory_resource* memory)
{
    if (bmd::code_language_by_file(file) != bmd::Code_Language::bms) {
        std::cout << ansi::red << "Error: file must have '.bms' suffix.\n";
        return 1;
    }
    std::optional<bmd::Code_Language> language = bmd::code_language_by_name(language_name);
    if (!language) {
        if (colors) {
            std::cout << ansi::red;
        }
        std::cout << "Error: unrecognized code language: '" << language_name << "'\n";
        return 1;
    }

    std::pmr::unsynchronized_pool_resource memory_resource(memory);
    const std::pmr::vector<char> source_data = load_file(file, &memory_resource);
    const std::string_view source { source_data.data(), source_data.size() };

    const std::pmr::vector<bms::Token> tokens = tokenize_bms_file(source, file, &memory_resource);
    bms::Parsed_Program p = parse_tokenized(tokens, source, file, &memory_resource);
    bms::Analyzed_Program a = analyze_parsed(p, file, &memory_resource);

    Code_String out { &memory_resource };
    int result_code = 0;
    if (Result<void, bmd::Generator_Error> r
        = bmd::generate_code(out, a, *language, &memory_resource, {});
        !r) {
        out.clear();
        print_generator_error(out, r.error());
        result_code = 1;
    }
    print_code_string(std::cout, out, colors);

    return result_code;
}

struct Command_Help {
    std::string_view name;
    std::string_view arguments;
    std::string_view description;
};

constexpr Command_Help command_helps[] {
    { "dump_tokens", "FILE", "Prints BMS/BMD tokens." },
    { "dump_ast", "FILE", "Prints the BMS/BMD abstract syntax tree." },
    { "verify", "FILE",
      "Performs semantic analysis on the BMS file, i.e. checks for correctness." },
    { "generate", "FILE LANGUAGE",
      "Converts the given BMS file to code in the specified language." },
    { "html", "FILE [OUTPUT_FILE]", "Converts the BMD file to an HTML file, or prints to stdout." }
};

void print_help(std::string_view program_name)
{
    std::cout << ansi::black << "Usage: " << ansi::reset << program_name //
              << ansi::yellow << " COMMAND " //
              << ansi::h_green << "...\n";
    for (const Command_Help& help : command_helps) {
        std::cout << "    " << ansi::yellow << help.name << " " //
                  << ansi::h_green << help.arguments << '\n' //
                  << "      " << ansi::reset << help.description << '\n';
    }
}

int main(int argc, const char** argv)
try {
    const std::vector<std::string_view> args(argv, argv + argc);
    const std::string_view program_name = args.size() == 0 ? "bitmanip" : args[0];

    if (args.size() < 3) {
        print_help(program_name);
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
    else if (args[1] == "generate") {
        if (args.size() < 4) {
            print_help(program_name);
            return 1;
        }
        return generate(args[2], args[3], &memory);
    }
    else if (args[1] == "html") {
        return to_html(args[2], argc > 3 ? args[3] : std::optional<std::string_view> {}, &memory);
    }
    else {
        std::cout << "Unknown command '" << args[1] << "'\n";
        return 1;
    }
} catch (const Assertion_Error& e) {
    bit_manipulation::Code_String out;
    print_assertion_error(out, e);
    print_code_string(std::cout, out, colors);
    return 1;
} catch (std::exception& e) {
    bit_manipulation::Code_String out;
    out.append("Unhandled exception! ", bit_manipulation::Code_Span_Type::diagnostic_error_text);
    out.append("An exception with the following message has been raised:",
               bit_manipulation::Code_Span_Type::diagnostic_text);
    out.append("\n\n");
    out.append(e.what(), bit_manipulation::Code_Span_Type::diagnostic_text);
    print_internal_error_notice(out);
    print_code_string(std::cout, out, colors);
    return 1;
} catch (...) {
    bit_manipulation::Code_String out;
    out.append("Unhandled exception! ", bit_manipulation::Code_Span_Type::diagnostic_error_text);
    out.append("An exception not derived from std::exception has been raised.",
               bit_manipulation::Code_Span_Type::diagnostic_text);
    out.append("\n\n");
    print_internal_error_notice(out);
    print_code_string(std::cout, out, colors);
    return 1;
}

} // namespace
} // namespace bit_manipulation

int main(int argc, const char** argv)
{
    return bit_manipulation::main(argc, argv);
}
