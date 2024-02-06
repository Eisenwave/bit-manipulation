#ifndef BIT_MANIPULATION_DIAGNOSTICS_HPP
#define BIT_MANIPULATION_DIAGNOSTICS_HPP

#include <iosfwd>
#include <span>
#include <string>
#include <string_view>

#include "ansi.hpp"
#include "assert.hpp"

#include "bms/analysis_error.hpp"
#include "bms/parse.hpp"
#include "bms/tokenize.hpp"
#include "io.hpp"

namespace bit_manipulation {

/// @brief Converts the given error code to a prose string which explains the problem.
/// @param  e the error code
/// @return The representative prose.
std::string_view to_prose(bms::Tokenize_Error_Code e);

/// @brief Converts the given error code to a prose string which explains the problem.
/// @param e the error code
/// @return The representative prose.
std::string_view to_prose(IO_Error_Code e);

/// @brief Returns the line that contains the given index.
/// @param source the source string
/// @param index the index within the source string, in range `[0, source.size())`
/// @return A line which contains the given `index`.
std::string_view find_line(std::string_view source, Size index);

/// @brief Prints the location of the file nicely formatted.
/// @param out the stream to write to.
/// @param file the file.
/// @return `out`
std::ostream& print_file_location(std::ostream& out, std::string_view file);

std::ostream&
print_file_position(std::ostream& out, std::string_view file, bms::Source_Position pos);

std::ostream&
print_affected_line(std::ostream& out, std::string_view source, bms::Source_Position pos);

std::ostream& print_tokenize_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   bms::Tokenize_Error e);

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                bms::Parse_Error e);

std::ostream& print_analysis_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   bms::Analysis_Error e);

std::ostream&
print_tokens(std::ostream& out, std::span<const bms::Token> tokens, std::string_view source);

std::ostream& print_ast(std::ostream& out, const bms::Parsed_Program& program, Size indent_width);

} // namespace bit_manipulation

#endif