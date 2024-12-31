#ifndef BIT_MANIPULATION_DIAGNOSTICS_HPP
#define BIT_MANIPULATION_DIAGNOSTICS_HPP

#include <iosfwd>
#include <span>
#include <string>
#include <string_view>

#include "common/ansi.hpp"
#include "common/assert.hpp"
#include "common/io.hpp"

#include "bms/fwd.hpp"

#include "bmd/fwd.hpp"

namespace bit_manipulation {

/// @brief Returns the line that contains the given index.
/// @param source the source string
/// @param index the index within the source string, in range `[0, source.size())`
/// @return A line which contains the given `index`.
std::string_view find_line(std::string_view source, Size index);

/// @brief Prints the location of the file nicely formatted.
/// @param out the string to write to
/// @param file the file
void print_location_of_file(Code_String& out, std::string_view file);

/// @brief Prints a position within a file, consisting of the file name and line/column.
/// @param out the string to write to
/// @param file the file
/// @param pos the position within the file
/// @param colon_suffix if `true`, appends a `:` to the string as part of the same token
void print_file_position(Code_String& out,
                         std::string_view file,
                         const Local_Source_Position& pos,
                         bool colon_suffix = true);

/// @brief Prints the contents of the affected line within `source` as well as position indicators
/// which show the span which is affected by some diagnostic.
/// @param out the string to write to
/// @param source the program source
/// @param pos the position within the source
void print_affected_line(Code_String& out,
                         std::string_view source,
                         const Local_Source_Position& pos);

void print_tokenize_error(Code_String& out,
                          std::string_view file,
                          std::string_view source,
                          const bms::Tokenize_Error& e);

void print_parse_error(Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bms::Parse_Error& e);

void print_analysis_error(Code_String& out,
                          const bms::Parsed_Program& program,
                          const bms::Analysis_Error& error);

void print_parse_error(Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bmd::Parse_Error& e);

void print_document_error(Code_String& out,
                          std::string_view file,
                          std::string_view source,
                          const bmd::Document_Error& error);

void print_generator_error(Code_String& out, const bmd::Generator_Error& error);

void print_assertion_error(Code_String& out, const Assertion_Error& error);

void print_io_error(Code_String& out, std::string_view file, IO_Error_Code error);

void print_tokens(Code_String& out, std::span<const bms::Token> tokens, std::string_view source);

struct BMS_AST_Formatting_Options {
    int indent_width;
};

void print_ast(Code_String& out, const bms::Parsed_Program& program, BMS_AST_Formatting_Options);

struct BMD_AST_Formatting_Options {
    int indent_width;
    int max_node_text_length;
};

void print_ast(Code_String& out, const bmd::Parsed_Document& document, BMD_AST_Formatting_Options);

void print_internal_error_notice(Code_String& out);

std::ostream& print_code_string(std::ostream& out, const Code_String& string, bool colors);

} // namespace bit_manipulation

#endif
