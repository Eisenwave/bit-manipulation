#include <iomanip>
#include <iostream>
#include <span>

#include "diagnostics.hpp"

#include "bms/tokens.hpp"

namespace bit_manipulation {

std::string_view to_prose(bms::Tokenize_Error_Code e)
{
    switch (e) {
    case bms::Tokenize_Error_Code::illegal_character: return "Illegal character encountered.";
    case bms::Tokenize_Error_Code::integer_suffix:
        return "Suffix after integer literal is not allowed";
    case bms::Tokenize_Error_Code::unterminated_comment:
        return "Unterminated block comment found. '/*' must have a matching '*/'";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view to_prose(IO_Error_Code e)
{
    switch (e) {
    case IO_Error_Code::cannot_open: return "Failed to open file.";
    case IO_Error_Code::read_error: return "I/O error occurred when reading from file.";
    case IO_Error_Code::write_error: return "I/O error occurred when writing to file.";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view find_line(std::string_view source, Size index)
{
    BIT_MANIPULATION_ASSERT(index < source.size());

    Size begin = source.rfind('\n', index);
    begin = begin != std::string_view::npos ? begin + 1 : 0;

    Size end = std::min(source.find('\n', index + 1), source.size());

    return source.substr(begin, end - begin);
}

std::ostream& print_file_location(std::ostream& out, std::string_view file)
{
    return out << ansi::black << file << ":" << ansi::reset;
}

std::ostream&
print_file_position(std::ostream& out, std::string_view file, bms::Source_Position pos)
{
    return out << ansi::black << file << ":" << pos.line + 1 << ":" << pos.column + 1
               << ansi::reset;
}

const std::string error_prefix = std::string(ansi::h_red) + "error: " + std::string(ansi::reset);

std::ostream&
print_affected_line(std::ostream& out, std::string_view source, bms::Source_Position pos)
{
    const std::string_view line = find_line(source, pos.begin);
    return out << std::right << std::setfill(' ') //
               << ansi::h_yellow << std::setw(5) << pos.line + 1 << ansi::reset << " |" //
               << line << '\n' //
               << std::setw(5) << ""
               << " |" //
               << std::string(pos.column, ' ') << ansi::h_green << "^\n"
               << ansi::reset;
}

std::ostream&
print_tokens(std::ostream& out, std::span<const bms::Token> tokens, std::string_view source)
{
    for (const bms::Token& t : tokens) {
        const std::string_view text = t.extract(source);
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
    return out;
}

std::ostream& print_tokenize_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   bms::Tokenize_Error e)
{
    print_file_position(out, file, e.pos);
    out << ": " << error_prefix << to_prose(e.code) << '\n';
    print_affected_line(out, source, e.pos);
    std::exit(1);
    return out;
}

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                bms::Parse_Error error)
{
    print_file_position(out, file, error.fail_token.pos);
    out << ": " << error_prefix //
        << "while matching '" << grammar_rule_name(error.fail_rule) << "', unexpected token "
        << token_type_readable_name(error.fail_token.type) << "\n";
    print_file_position(out, file, error.fail_token.pos);
    out << ": note: expected ";

    const std::span<const bms::Token_Type> expected = error.expected_tokens;
    if (expected.size() == 0) {
        out << "nothing";
    }
    else if (expected.size() == 1) {
        out << token_type_readable_name(expected[0]);
    }
    else {
        out << "one of: ";
        for (Size i = 0; i < expected.size(); ++i) {
            out << (i + 1 == expected.size() ? ", or " : i != 0 ? ", " : "");
            out << token_type_readable_name(expected[i]);
        }
    }
    out << "\n";
    return print_affected_line(out, source, error.fail_token.pos);
}

} // namespace bit_manipulation