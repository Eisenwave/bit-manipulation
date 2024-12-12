#ifndef BIT_MANIPULATION_DIAGNOSTICS_HPP
#define BIT_MANIPULATION_DIAGNOSTICS_HPP

#include <iosfwd>
#include <span>
#include <string>
#include <string_view>

#include "common/ansi.hpp"
#include "common/assert.hpp"
#include "common/io.hpp"

#include "bmd/fwd.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation {

/// @brief Returns the line that contains the given index.
/// @param source the source string
/// @param index the index within the source string, in range `[0, source.size())`
/// @return A line which contains the given `index`.
std::string_view find_line(std::string_view source, Size index);

/// @brief Prints the location of the file nicely formatted.
/// @param out the string to write to
/// @param file the file
void print_location_of_file(bmd::Code_String& out, std::string_view file);

/// @brief Prints the location of the file nicely formatted.
/// @param out the stream to write to
/// @param file the file
/// @param colors if `true`, adds ANSI color
/// @return `out`
std::ostream& print_location_of_file(std::ostream& out, std::string_view file, bool colors);

/// @brief Prints a position within a file, consisting of the file name and line/column.
/// @param out the string to write to
/// @param file the file
/// @param pos the position within the file
/// @param colon_suffix if `true`, appends a `:` to the string as part of the same token
void print_file_position(bmd::Code_String& out,
                         std::string_view file,
                         const Local_Source_Position& pos,
                         bool colon_suffix);

/// @brief Prints a position within a file, consisting of the file name and line/column.
/// @param out the stream to write to
/// @param file the file
/// @param pos the position within the file
/// @param colors if `true`, adds ANSI color
/// @return `out`
std::ostream& print_file_position(std::ostream& out,
                                  std::string_view file,
                                  const Local_Source_Position& pos,
                                  bool colors);

void print_affected_line(bmd::Code_String& out,
                         std::string_view source,
                         const Local_Source_Position& pos);

/// @brief Prints the contents of the affected line within `source` as well as position indicators
/// which show the span which is affected by some diagnostic.
/// @param out the stream to write to
/// @param source the program source
/// @param pos the position within the source
/// @param colors if `true`, adds ANSI color
/// @return `out`
std::ostream& print_affected_line(std::ostream& out,
                                  std::string_view source,
                                  const Local_Source_Position& pos,
                                  bool colors);

void print_tokenize_error(bmd::Code_String& out,
                          std::string_view file,
                          std::string_view source,
                          const bms::Tokenize_Error& e);

std::ostream& print_tokenize_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   const bms::Tokenize_Error& e,
                                   bool colors);

void print_parse_error(bmd::Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bms::Parse_Error& e);

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                const bms::Parse_Error& e,
                                bool colors);

void print_analysis_error(bmd::Code_String& out,
                          const bms::Parsed_Program& program,
                          const bms::Analysis_Error& error);

std::ostream& print_analysis_error(std::ostream& out,
                                   const bms::Parsed_Program& program,
                                   const bms::Analysis_Error& error,
                                   bool colors);

void print_parse_error(bmd::Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bmd::Parse_Error& e);

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                const bmd::Parse_Error& e,
                                bool colors);

void print_document_error(bmd::Code_String out,
                          std::string_view file,
                          std::string_view source,
                          const bmd::Document_Error& error);

std::ostream& print_document_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   const bmd::Document_Error& error,
                                   bool colors);

void print_assertion_error(bmd::Code_String& out, const Assertion_Error& error);

std::ostream& print_assertion_error(std::ostream& out, const Assertion_Error& error, bool colors);

void print_io_error(bmd::Code_String& out, std::string_view file, IO_Error_Code error);

std::ostream&
print_io_error(std::ostream& out, std::string_view file, IO_Error_Code error, bool colors);

// TODO: implement
void print_tokens(bmd::Code_String& out,
                  std::span<const bms::Token> tokens,
                  std::string_view source)
    = delete;

std::ostream&
print_tokens(std::ostream& out, std::span<const bms::Token> tokens, std::string_view source);

struct BMS_AST_Formatting_Options {
    int indent_width;
    bool colors;
};

// TODO: implement
void print_ast(bmd::Code_String& out,
               const bms::Parsed_Program& program,
               BMS_AST_Formatting_Options)
    = delete;

std::ostream&
print_ast(std::ostream& out, const bms::Parsed_Program& program, BMS_AST_Formatting_Options);

struct BMD_AST_Formatting_Options {
    int indent_width;
    int max_node_text_length;
    bool colors;
};

// TODO: implement
void print_ast(bmd::Code_String& out,
               const bmd::Parsed_Document& document,
               BMD_AST_Formatting_Options)
    = delete;

std::ostream&
print_ast(std::ostream& out, const bmd::Parsed_Document& document, BMD_AST_Formatting_Options);

void print_internal_error_notice(bmd::Code_String& out);

std::ostream& print_internal_error_notice(std::ostream& out, bool colors);

} // namespace bit_manipulation

#endif
