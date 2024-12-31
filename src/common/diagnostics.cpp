#include <charconv>
#include <functional>
#include <span>

#include "common/code_string.hpp"
#include "common/diagnostics.hpp"
#include "common/to_chars.hpp"

#include "bms/analysis_error.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_value.hpp"
#include "bms/evaluation/evaluation_error.hpp"
#include "bms/evaluation/operations.hpp"
#include "bms/fwd.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/parsing/parse_error.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/tokenize_error.hpp"
#include "bms/vm/execution_error.hpp"

#include "bmd/codegen/generator_error.hpp"
#include "bmd/html/doc_to_html.hpp"
#include "bmd/html/html_writer.hpp"
#include "bmd/parsing/ast.hpp"
#include "bmd/parsing/grammar.hpp"
#include "bmd/parsing/parse.hpp"

namespace bit_manipulation {

namespace {

[[nodiscard]] std::string_view highlight_color_of(Code_Span_Type type)
{
    using enum Code_Span_Type;
    switch (type) {
    case text: return ansi::reset;

    case identifier:
    case variable_name:
    case function_name: return ansi::h_white;

    case annotation_name:
    case type_name: return ansi::h_blue;

    case number: return ansi::h_cyan;

    case string: return ansi::h_green;

    case comment:
    case operation: return ansi::h_black;

    case bracket:
    case punctuation: return ansi::black;

    case keyword:
    case boolean_literal: return ansi::h_magenta;

    case error: return ansi::h_red;

    case diagnostic_text:
    case diagnostic_code_citation:
    case diagnostic_punctuation:
    case diagnostic_operator: return ansi::reset;

    case diagnostic_code_position:
    case diagnostic_internal: return ansi::h_black;

    case diagnostic_error_text:
    case diagnostic_error: return ansi::h_red;

    case diagnostic_warning:
    case diagnostic_line_number: return ansi::h_yellow;

    case diagnostic_note: return ansi::h_white;

    case diagnostic_position_indicator: return ansi::h_green;

    case diagnostic_internal_error_notice: return ansi::h_yellow;

    case diagnostic_operand: return ansi::h_magenta;

    case diagnostic_tag: return ansi::h_blue;

    case diagnostic_attribute: return ansi::h_magenta;

    case diagnostic_escape: return ansi::h_yellow;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown code span type.");
}

void append_compared_value(Code_String& out, bms::Concrete_Value v)
{
    constexpr auto span_type = Code_Span_Type::diagnostic_operand;

    switch (v.type.type()) {
    case bms::Type_Type::Nothing: //
        out.append("Nothing", span_type);
        return;
    case bms::Type_Type::Void: //
        out.append("Void", span_type);
        return;
    case bms::Type_Type::Bool: //
        out.append(v.int_value ? "true" : "false", span_type);
        return;
    case bms::Type_Type::Int: //
        out.append_integer(v.int_value, span_type);
        return;
    case bms::Type_Type::Uint: //
        out.append_integer(Big_Uint(v.int_value), span_type);
        return;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type");
}

enum struct Error_Line_Type : Default_Underlying { note, error };

struct Error_Line {
    std::optional<Source_Position> pos {};
    std::string_view message;
    bool omit_affected_line = false;
};

// Certain internal errors aren't assertion failures but make it here.
// For example, an evaluation error can fail as a type error, but that can only happen
// if prior analysis didn't spot the type error in the first place.
// Similarly, VM execution errors are also "clean errors", but only end up here as the result
// of faulty codegen.
[[nodiscard]] bool is_internal(const bms::Analysis_Error& error)
{
    return error.code() == bms::Analysis_Error_Code::execution_error
        || (error.code() == bms::Analysis_Error_Code::evaluation_error
            && error.evaluation_error() == bms::Evaluation_Error_Code::type_error);
}

constexpr std::string_view error_prefix = "error:";
constexpr std::string_view note_prefix = "note:";

[[nodiscard]] std::string_view to_prose(bms::Tokenize_Error_Code e)
{
    using enum bms::Tokenize_Error_Code;
    switch (e) {
    case illegal_character: //
        return "Illegal character encountered.";
    case no_digits_following_integer_prefix:
        return "Digits after integer prefix are required; for example '0b01', '0xff', etc.";
    case integer_suffix: //
        return "Suffix after integer literal is not allowed";
    case unterminated_comment: //
        return "Unterminated block comment found. '/*' must have a matching '*/'";
    case unterminated_string: //
        return "Unterminated string found. Each opening '\"' must have a closing '\"'.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

[[nodiscard]] std::string_view to_prose(bms::Analysis_Error_Code e)
{
    using enum bms::Analysis_Error_Code;
    switch (e) {
    case failed_to_define_global_const: //
        return "A name was already in use when attempting to define a global constant.";
    case failed_to_define_function: //
        return "A name was already in use when attempting to define a function.";
    case failed_to_define_parameter: //
        return "A name was already in use when attempting to define a function parameter.";
    case failed_to_define_variable: //
        return "A name was already in use when attempting to define a variable.";
    case reference_to_undefined_variable: //
        return "An expression attempted to look up a variable or parameter, but it was not "
               "defined.";
    case assignment_of_undefined_variable: //
        return "An assignment attempted to assign a variable which is not defined.";
    case call_to_undefined_function: //
        return "A function call to an undefined function was attempted.";
    case width_not_integer: //
        return "The width of Uint must be an integer.";
    case width_invalid: //
        static_assert(uint_max_width == 128);
        return "The width of Uint must be in range [0, 128).";
    case expected_constant_expression: //
        return "Expected a constant expression, but was unable to perform constant folding.";
    case let_variable_in_constant_expression: //
        return "Cannot use variables in a constant expression. Did you mean 'const'?";
    case parameter_in_constant_expression: //
        return "Cannot use function parameters in a constant expression.";
    case function_in_expression: //
        return "Attempted to use a function in an expression as if it was a variable.";
    case execution_error: //
        return "Error in the execution of the generated code for constant-evaluated functions.";
    case no_return: //
        return "This function might not return. Ensure returning, or specify a Void return type.";
    case unreachable_code: //
        return "This statement is unreachable.";
    case evaluation_error: //
        return "Evaluation error in constant expressions or constant folding.";
    case condition_not_bool: //
        return "Condition of an if statement or while loop must be of type 'Bool'.";
    case invalid_integer_literal: //
        return "The given literal is invalid, possibly because it is too large for the compiler's "
               "internal integer representation.";
    case assigning_parameter: //
        return "Cannot assign a function parameter.";
    case assigning_function: //
        return "Cannot assign a function.";
    case assigning_const: //
        return "Cannot assign a constant. Did you mean to use 'let'?";
    case call_non_function: //
        return "Cannot call something that is not a function.";
    case wrong_number_of_arguments: //
        return "Wrong number of arguments provided to function.";
    case codegen_call_to_unanalyzed: //
        return "Constant evaluation depends on a function whose definition is not yet complete.";
    case width_deduction_from_non_uint: //
        return "Cannot deduce the width of Uint from a type that is not Uint.";
    case static_assert_expression_not_bool: //
        return "The expression of a static assertion must be of type 'Bool'.";
    case static_assertion_failed: //
        return "Static assertion failed.";
    case requires_clause_not_bool: //
        return "The expression in a requires-clause must be of type 'Bool'.";
    case requires_clause_not_satisfied: //
        return "Requires-clause was not satisfied.";
    case use_of_undefined_variable: //
        return "Use of undefined variable.";
    case use_of_undefined_constant: //
        return "Use of undefined constant.";
    case empty_return_in_non_void_function: //
        return "Cannot return nothing in a non-void function.";
    case invalid_operator: //
        return "Invalid operator is used.";
    case void_operation: //
        return "Cannot perform operations for operands of type 'Void'.";
    case bool_arithmetic: //
        return "Cannot use arithmetic operators for operands of type 'Bool'.";
    case bool_bitwise: //
        return "Cannot use bitwise operators for operands of type 'Bool'.";
    case bool_relational_comparison: //
        return "Cannot use relational comparisons between operands of type 'Bool'."
               " Only logical comparisons are allowed.";
    case int_bitwise: //
        return "Cannot use bitwise operators for operands of type 'Int'.";
    case int_logical: //
        return "Cannot use logical operators for operands of type 'Int'.";
    case uint_logical: //
        return "Cannot us logical operators for operands of type 'Uint'.";
    case non_bool_logical: //
        return "Logical operators can only be applied to 'Bool'.";
    case incompatible_types: //
        return "Incompatible types for operation or conversion.";
    case incompatible_widths: //
        return "Incompatible Uint widths for operation or conversion.";
    case wrong_argument_type: //
        return "Wrong argument types for function call.";
    case break_outside_loop: //
        return "This break statement is not inside of a loop.";
    case continue_outside_loop: //
        return "This continue statement is not inside of a loop.";
    case execution_limit_exceeded: //
        return "Evaluation of this constant expression took too long.";
    case annotation_unknown: //
        return "Unknown annotation.";
    case annotation_duplicate: //
        return "The same annotation cannot be applied twice.";
    case annotation_not_applicable: //
        return "Annotation cannot be applied to the following construct.";
    case annotation_too_many_arguments: //
        return "Too many arguments for provided for this annotation.";
    case annotation_unknown_parameter: //
        return "This argument does not match any annotation parameter.";
    case annotation_argument_duplicate: //
        return "This argument was provided twice for the same parameter.";
    case annotation_argument_wrong_type: //
        return "The argument type is invalid for the parameter.";
    case annotation_argument_wrong_value: //
        return "This argument value is not allowed.";
    case annotation_missing_argument: //
        return "The annotation is missing a required argument.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

[[nodiscard]] std::string_view to_prose(bms::Evaluation_Error_Code e)
{
    using enum bms::Evaluation_Error_Code;
    switch (e) {
    case type_error: //
        return "The evaluation violates the type system.";
    case division_by_zero: //
        return "Division by zero.";
    case shift_too_much: //
        return "Bit-shift was not less than the operand size.";
    case assertion_fail: //
        return "Assertion failed.";
    case unreachable: //
        return "Call to unreachable().";
    case int_to_uint_range_error: //
        return "Conversion from Int to Uint would lose information.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

[[nodiscard]] std::string_view to_prose(bms::Execution_Error_Code e)
{
    using enum bms::Execution_Error_Code;
    switch (e) {
    case load_uninitialized: //
        return "Load from uninitialized variable.";
    case pop: //
        return "Pop from empty stack.";
    case pop_call: //
        return "Return from outside a function.";
    case evaluation: //
        return "Error in evaluation of operator.";
    case jump_out_of_program: //
        return "Jump to address which is outside the program.";
    case jump_if_not_bool: //
        return "Condition of Jump_If instruction is not of type 'Bool'.";
    case symbolic_jump: //
        return "Execution of a symbolic jump.";
    case call_out_of_program: //
        return "Call of a function address which is outside the program.";
    case infinite_loop: //
        return "Infinite loop in VM execution.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

[[nodiscard]] std::string_view to_prose(bmd::Parse_Error_Code e)
{
    using enum bmd::Parse_Error_Code;
    switch (e) {
    case unexpected_character: //
        return "Encountered unexpected character while parsing.";
    case unexpected_eof: //
        return "Unexpected end of file.";
    case unterminated_comment: //
        return "Unterminated comment.";
    case invalid_integer_literal: //
        return "Invalid integer literal.";
    case integer_suffix: //
        return "Suffixes after integer literals are not allowed. Did you forget a space, comma, "
               "etc.?";
    case invalid_directive: //
        return "Invalid directive.";
    case duplicate_argument: //
        return "Duplicate argument in directive argument list.";
    case directive_must_be_empty: //
        return "This directive must have an empty block or no block at all.";
    case paragraph_break_in_span: //
        return "Paragraph breaks (blank lines) are not allowed in this directive.";
    case directive_in_text_span: //
        return "Only plaintext is allowed here, no directives.";
    case text_in_directive_list: //
        return "Only directives are allowed here, no plaintext.";
    case directive_not_allowed: //
        return "This directive is not allowed here.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid error code.");
}

[[nodiscard]] std::string_view to_prose(bmd::Document_Error_Code e)
{
    using enum bmd::Document_Error_Code;
    switch (e) {
    case writer_misuse: //
        return "The internal HTML writer has been misused by the developer.";
    case directive_not_allowed: //
        return "This directive is not allowed here.";
    case meta_not_at_start_of_file: //
        return "A \\meta directive must be at the beginning of the document.";
    case duplicate_meta_entry: //
        return "Each entry within a \\meta directive must be unique, but an entry was duplicated.";
    case invalid_language: //
        return "This language is not valid or not supported for syntax highlighting.";
    case invalid_architecture: //
        return "This architecture is not valid.";
    case number_attribute_not_allowed: //
        return "This annotation cannot have a numeric value; only strings can be provided.";
    case code_tokenization_failure: //
        return "Syntax highlighting for this code snipped failed. Is it ill-formed? Consider using "
               "'text' language or '\\tt' (teletype), not '\\c'.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid error code.");
}

[[nodiscard]] std::string_view to_prose(bmd::Generator_Error_Code e)
{
    using enum bmd::Generator_Error_Code;
    switch (e) {
    case unsupported_language: //
        return "The specified target language is not supported.";
    case empty: //
        return "No code was generated for this code construct.";
    case unsupported_integer_width: //
        return "An integer with an unsupported width was requested.";
    case error: //
        return "This code construct is untranslatable to the target language.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid error code.");
}

[[nodiscard]] std::string_view cause_to_prose(bms::Analysis_Error_Code e)
{
    using enum bms::Analysis_Error_Code;
    switch (e) {
    case failed_to_define_global_const:
    case failed_to_define_function:
    case failed_to_define_parameter:
    case failed_to_define_variable: //
        return "An entity with the same name is already defined here:";
    case width_not_integer: //
        return "The following expression must be of type 'Int':";
    case width_invalid: //
        return "The following expression results in invalid width:";
    case let_variable_in_constant_expression: //
        return "The referenced variable is declared 'let', here:";
    case parameter_in_constant_expression: //
        return "The referenced entity is defined as a function parameter here:";
    case function_in_expression: //
        return "The referenced entity is defined as a function here:";
    case condition_not_bool: //
        return "The following expression must be of type 'Bool':";
    case assigning_parameter: //
        return "The assigned parameter is defined here:";
    case assigning_function: //
        return "The assigned function is defined here:";
    case assigning_const: //
        return "The assigned object is declared 'const' here:";
    case call_non_function: //
        return "The called entity is declared here:";
    case wrong_number_of_arguments: //
        return "The arguments must match the following parameter list:";
    case codegen_call_to_unanalyzed: //
        return "The following function was called:";
    case width_deduction_from_non_uint: //
        return "The following expression is not of type 'Uint':";
    case static_assert_expression_not_bool: //
        return "The following expression must be of type 'Bool':";
    case static_assertion_failed: //
        return "The following expression must evaluate to 'true', but evaluated to 'false':";
    case requires_clause_not_bool:
    case requires_clause_not_satisfied: //
        return "Requires-clause is attached to this function:";
    case use_of_undefined_variable: //
        return "The following variable is undefined before its first use:";
    case use_of_undefined_constant: //
        return "The following constant is undefined before its first use:";
    case empty_return_in_non_void_function: //
        return "The function's return type is not declared Void:";
    case execution_limit_exceeded:
        return "Evaluation was abandoned here: (could this be an infinite loop?)";
    case unreachable_code: //
        return "See prior statement which definitely returns here:";
    case annotation_unknown: //
        return "When applying annotation here:";
    case annotation_duplicate: //
        return "Annotation has been previously specified here:";
    case annotation_not_applicable: //
        return "Annotation cannot be applied here:";
    case annotation_too_many_arguments:
    case annotation_unknown_parameter:
    case annotation_argument_duplicate:
    case annotation_argument_wrong_type:
    case annotation_argument_wrong_value: //
        return "In this annotation:";
    default: //
        return "Caused by:";
    }
}

[[nodiscard]] std::string_view to_prose(IO_Error_Code e)
{
    using enum IO_Error_Code;
    switch (e) {
    case cannot_open: //
        return "Failed to open file.";
    case read_error: //
        return "I/O error occurred when reading from file.";
    case write_error: //
        return "I/O error occurred when writing to file.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

[[nodiscard]] bool is_incompatible_return_type_error(const bms::Analysis_Error& error)
{
    return error.code() == bms::Analysis_Error_Code::incompatible_types
        && error.fail().construct == bms::Construct::return_statement;
}

[[nodiscard]] std::string_view diagnosis_name_of(bms::Annotation_Parameter_Type type)
{
    using enum bms::Annotation_Parameter_Type;
    switch (type) {
    case boolean: return "boolean (true/false)";
    case integer: return "an integer (1, 0x1, ...)";
    case string: return "a string (\"...\")";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type.");
}

[[nodiscard]] std::string_view diagnosis_name_of(bms::Construct construct)
{
    using enum bms::Construct;
    switch (construct) {
    case program: return "program";
    case function: return "function";
    case parameter: return "function parameter";
    case type: return "type";
    case implicit_type: return "implicit type";
    case constant: return "constant";
    case variable: return "variable";
    case static_assertion: return "static assertion";
    case if_statement: return "if-statement";
    case while_statement: return "while-statement";
    case break_statement: return "break-statement";
    case continue_statement: return "continue-statement";
    case return_statement: return "return-statement";
    case assignment: return "assignment";
    case block_statement: return "block statement";
    case conversion_expression: return "conversion expression";
    case if_expression: return "if expression";
    case binary_expression: return "binary expression";
    case prefix_expression: return "prefix unary expression";
    case function_call_expression: return "function call expression";
    case id_expression: return "id-expression";
    case literal: return "literal";
    case builtin_function: return "builtin function";
    case annotation: return "annotation";
    case annotation_argument: return "annotation argument";
    case annotation_parameter: return "annotation parameter";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid construct.");
}

void print_source_position(Code_String& out, const std::optional<Source_Position>& pos)
{
    if (!pos) {
        out.append("(internal):", Code_Span_Type::diagnostic_code_position);
    }
    else {
        print_file_position(out, pos->file_name, Local_Source_Position { *pos });
    }
}

void print_diagnostic_prefix(Code_String& out,
                             Error_Line_Type type,
                             std::optional<Source_Position> pos)
{
    print_source_position(out, pos);
    out.append(' ');
    switch (type) {
    case Error_Line_Type::error: //
        out.append(error_prefix, Code_Span_Type::diagnostic_error);
        break;
    case Error_Line_Type::note: //
        out.append(note_prefix, Code_Span_Type::diagnostic_note);
        break;
    }
}

void print_diagnostic_line(Code_String& out,
                           Error_Line_Type type,
                           const Error_Line& line,
                           std::string_view source)
{
    print_diagnostic_prefix(out, type, line.pos);
    out.append(' ');
    out.append(line.message, Code_Span_Type::diagnostic_text);

    out.append('\n');
    if (line.pos && !line.omit_affected_line) {
        print_affected_line(out, source, *line.pos);
    }
}

void print_error_line(Code_String& out, const Error_Line& line, std::string_view source)
{
    return print_diagnostic_line(out, Error_Line_Type::error, line, source);
}

void print_note_line(Code_String& out, const Error_Line& line, std::string_view source)
{
    return print_diagnostic_line(out, Error_Line_Type::note, line, source);
}

} // namespace

void print_file_position(Code_String& out,
                         std::string_view file,
                         const Local_Source_Position& pos,
                         bool suffix_colon)
{
    auto builder = out.build(Code_Span_Type::diagnostic_code_position);
    builder.append(file)
        .append(':')
        .append_integer(pos.line + 1)
        .append(':')
        .append_integer(pos.column + 1);
    if (suffix_colon) {
        builder.append(':');
    }
}

std::string_view find_line(std::string_view source, Size index)
{
    BIT_MANIPULATION_ASSERT(index <= source.size());

    if (index == source.size() || source[index] == '\n') {
        // Special case for EOF positions, which may be past the end of a line,
        // and even past the end of the whole source, but only by a single character.
        // For such positions, we yield the currently ended line.
        --index;
    }

    Size begin = source.rfind('\n', index);
    begin = begin != std::string_view::npos ? begin + 1 : 0;

    Size end = std::min(source.find('\n', index + 1), source.size());

    return source.substr(begin, end - begin);
}

void print_location_of_file(Code_String& out, std::string_view file)
{
    out.build(Code_Span_Type::diagnostic_code_position).append(file).append(':');
}

void print_affected_line(Code_String& out,
                         std::string_view source,
                         const Local_Source_Position& pos)
{
    const std::string_view line = find_line(source, pos.begin);

    Characters line_chars = to_characters(pos.line + 1);
    constexpr Size pad_max = 6;
    Size pad_length = pad_max - std::min(line_chars.length, Size { pad_max - 1 });
    out.append(pad_length, ' ');
    out.append_integer(pos.line + 1, Code_Span_Type::diagnostic_line_number);
    out.append(' ');
    out.append('|', Code_Span_Type::diagnostic_punctuation);
    out.append(' ');
    out.append(line, Code_Span_Type::diagnostic_code_citation);
    out.append('\n');

    Size align_length = std::max(pad_max, line_chars.length + 1);
    out.append(align_length, ' ');
    out.append(' ');
    out.append('|', Code_Span_Type::diagnostic_punctuation);
    out.append(' ');
    out.append(pos.column, ' ');
    out.append('^', Code_Span_Type::diagnostic_position_indicator);
    out.append('\n');
}

void print_tokenize_error(Code_String& out,
                          std::string_view file,
                          std::string_view source,
                          const bms::Tokenize_Error& e)
{
    print_file_position(out, file, e.pos);
    out.append(' ');
    out.append(error_prefix, Code_Span_Type::diagnostic_error);
    out.append(' ');
    out.append(to_prose(e.code), Code_Span_Type::diagnostic_text);
    out.append('\n');
    print_affected_line(out, source, e.pos);
}

void print_parse_error(Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bms::Parse_Error& error)
{
    print_file_position(out, file, error.fail_token.pos);
    out.append(' ');
    out.append(error_prefix, Code_Span_Type::diagnostic_error);
    out.append(' ');

    const std::string_view preamble
        = error.fail_token.type == bms::Token_Type::eof ? "unexpected " : "unexpected token ";
    out.build(Code_Span_Type::diagnostic_text)
        .append(preamble)
        .append(token_type_readable_name(error.fail_token.type))
        .append(" while matching '")
        .append(grammar_rule_name(error.fail_rule))
        .append('\'');
    out.append('\n');

    print_file_position(out, file, error.fail_token.pos);
    out.append(' ');
    out.append(note_prefix, Code_Span_Type::diagnostic_note);
    out.append(' ');

    {
        auto note_builder = out.build(Code_Span_Type::diagnostic_text);
        note_builder.append("expected ");

        const std::span<const bms::Token_Type> expected = error.expected_tokens;
        if (expected.size() == 0) {
            note_builder.append("nothing");
        }
        else if (expected.size() == 1) {
            note_builder.append(token_type_readable_name(expected[0]));
        }
        else {
            note_builder.append("one of: ");
            for (Size i = 0; i < expected.size(); ++i) {
                note_builder.append(i + 1 == expected.size() ? ", or " : i != 0 ? ", " : "");
                note_builder.append(token_type_readable_name(expected[i]));
            }
        }
    }
    out.append('\n');
    print_affected_line(out, source, error.fail_token.pos);
}

void print_parse_error(Code_String& out,
                       std::string_view file,
                       std::string_view source,
                       const bmd::Parse_Error& error)
{
    print_file_position(out, file, error.pos);
    out.append(error_prefix, Code_Span_Type::diagnostic_error);
    out.append(' ');

    if (error.code == bmd::Parse_Error_Code::unexpected_character) {
        out.build(Code_Span_Type::diagnostic_text)
            .append("unexpected character '")
            .append(source[error.pos.begin])
            .append("' while matching '")
            .append(grammar_rule_name(error.rule))
            .append('\'');
        out.append('\n');
        // TODO: diagnose expected character perhaps
    }
    else {
        out.append(to_prose(error.code), Code_Span_Type::diagnostic_text);
    }
    out.append('\n');

    if (!source.empty()) {
        print_affected_line(out, source, error.pos);
    }
}

void print_analysis_error(Code_String& out,
                          const bms::Parsed_Program& program,
                          const bms::Analysis_Error& error)
{
    if (is_incompatible_return_type_error(error)) {
        BIT_MANIPULATION_ASSERT(error.cause());
        BIT_MANIPULATION_ASSERT(error.type());
        BIT_MANIPULATION_ASSERT(construct_is_type(*error.cause_construct()));

        const bool is_void = error.type() == bms::Concrete_Type::Void;

        print_error_line(
            out,
            { error.fail_pos(),
              is_void ? "Cannot have non-empty return statement in a function returning Void."
                      : "Invalid conversion between return statement and return type." },
            program.get_source());
        if (is_void) {
            print_note_line(out,
                            { error.fail_pos(), "Use 'return;' to return from a Void function." },
                            program.get_source());
        }
        print_note_line(
            out,
            { error.cause_pos(),
              error.cause_construct() == bms::Construct::implicit_type
                  ? "This function implicitly returns Void because no return type was specified:"
                  : "Return type is declared here:" },
            program.get_source());
        return;
    }
    if (error.code() == bms::Analysis_Error_Code::width_invalid) {
        const std::optional<bms::Value>& width_value = error.value();
        // if width_invalid got raised during analysis, it means we have to know the width
        BIT_MANIPULATION_ASSERT(width_value);
        const Big_Int width = width_value->as_int();
        std::string prefix = "The width evaluated to ";
        prefix += to_characters(width).as_string();
        if (width < 0) {
            print_error_line(
                out, { error.fail_pos(), std::move(prefix) + ", but widths must be positive." },
                program.get_source());
        }
        else if (width == 0) {
            print_error_line(
                out, { error.fail_pos(), std::move(prefix) + ", but widths shall not be zero." },
                program.get_source());
        }
        else if (width > uint_max_width) {
            std::string message = std::move(prefix);
            message += ", but the maximum allowed is ";
            message += to_characters(uint_max_width).as_string();
            message += '.';
            print_error_line(out, { error.fail_pos(), message }, program.get_source());
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("width_invalid raised for seemingly no reason");
        }
        return;
    }

    print_error_line(out, { error.fail_pos(), to_prose(error.code()) }, program.get_source());

    if (error.code() == bms::Analysis_Error_Code::execution_error) {
        print_note_line(out, { error.fail_pos(), to_prose(error.execution_error()) },
                        program.get_source());
    }

    if (const auto wrong_argument = error.wrong_argument()) {
        std::string message = "Parameter '";
        message += wrong_argument->name;
        message += "' must be ";
        switch (error.code()) {
        case bms::Analysis_Error_Code::annotation_argument_wrong_type:
            message += diagnosis_name_of(wrong_argument->expected);
            break;
        case bms::Analysis_Error_Code::annotation_argument_wrong_value:
            message += wrong_argument->value_constraints;
            break;
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Don't know what to do with wrong_argument().");
        }
        print_note_line(out,
                        { .pos = error.fail_pos(), .message = message, .omit_affected_line = true },
                        program.get_source());
    }

    if (const auto comp_fail = error.comparison_failure()) {
        print_diagnostic_prefix(out, Error_Line_Type::note, error.cause_pos());
        out.append(' ');
        out.append("Comparison evaluated to ", Code_Span_Type::diagnostic_text);
        append_compared_value(out, comp_fail->left);
        out.append(' ');
        out.append(expression_type_code_name(comp_fail->op), Code_Span_Type::diagnostic_operator);
        out.append(' ');
        append_compared_value(out, comp_fail->right);
        out.append('\n');
        if (std::optional<Source_Position> pos = error.cause_pos()) {
            print_affected_line(out, program.get_source(), *pos);
        }
    }
    else if (auto cause = error.cause()) {
        std::string message = error.code() == bms::Analysis_Error_Code::evaluation_error
            ? "Caused by: " + std::string(to_prose(error.evaluation_error()))
            : std::string(cause_to_prose(error.code()));
        if (!cause->pos) {
            message += " (";
            message += diagnosis_name_of(cause->construct);
            if (!cause->name.empty()) {
                message += " '";
                message += cause->name;
                message += '\'';
            }
            message += ')';
        }

        print_note_line(out, { cause->pos, message }, program.get_source());
    }

    if (is_internal(error)) {
        print_internal_error_notice(out);
    }
}

void print_document_error(Code_String& out,
                          std::string_view file,
                          std::string_view source,
                          const bmd::Document_Error& error)
{
    print_file_position(out, file, error.pos);
    out.append(' ');
    out.append(to_prose(error.code), Code_Span_Type::diagnostic_text);

    print_affected_line(out, source, error.pos);
    if (error.code == bmd::Document_Error_Code::writer_misuse) {
        print_internal_error_notice(out);
    }
}

void print_generator_error(Code_String& out, const bmd::Generator_Error& error)
{
    auto pos = error.fail ? std::optional<Source_Position> {} : get_source_position(*error.fail);
    print_source_position(out, pos);
    out.append(' ');
    out.append(to_prose(error.code), Code_Span_Type::diagnostic_text);
    out.append('\n');
}

void print_assertion_error(Code_String& out, const Assertion_Error& error)
{
    out.append("Assertion failed! ", Code_Span_Type::diagnostic_error);

    const std::string_view message = error.type == Assertion_Error_Type::expression
        ? "The following expression evaluated to 'false', but was expected to be 'true':"
        : "Code which must be unreachable has been reached.";
    out.append(message, Code_Span_Type::diagnostic_text);
    out.append("\n\n");

    Local_Source_Position pos { .line = error.location.line(),
                                .column = error.location.column(),
                                .begin = {} };
    print_file_position(out, error.location.file_name(), pos);
    out.append(' ');
    out.append(error.message, Code_Span_Type::diagnostic_error_text);
    out.append("\n\n");
    print_internal_error_notice(out);
}

void print_io_error(Code_String& out, std::string_view file, IO_Error_Code error)
{
    print_location_of_file(out, file);
    out.append(' ');
    out.append(to_prose(error), Code_Span_Type::diagnostic_text);
    out.append('\n');
}

void print_tokens(Code_String& out, std::span<const bms::Token> tokens, std::string_view source)
{
    for (const bms::Token& t : tokens) {
        {
            constexpr Size align_size = 2;
            auto pos_builder = out.build(Code_Span_Type::diagnostic_code_position);
            Characters line_chars = to_characters(t.pos.line + 1);
            pos_builder.append(align_size - std::min(line_chars.length, align_size), ' ');
            pos_builder.append(line_chars.as_string());
            pos_builder.append(':');

            Characters column_chars = to_characters(t.pos.column + 1);
            pos_builder.append(align_size - std::min(column_chars.length, align_size), ' ');
            pos_builder.append(column_chars.as_string());
            pos_builder.append(':');
        }

        out.append(' ');
        out.append(token_type_name(t.type), Code_Span_Type::diagnostic_tag);

        if (token_type_length(t.type) == 0) {
            const std::string_view text = source.substr(t.pos.begin, t.pos.length);
            out.append('(', Code_Span_Type::diagnostic_punctuation);
            out.append(text, Code_Span_Type::diagnostic_code_citation);
            out.append(')', Code_Span_Type::diagnostic_punctuation);

            if (text.length() > 1) {
                out.append(' ');
                auto builder = out.build(Code_Span_Type::diagnostic_text);
                builder.append('(').append_integer(text.length()).append(" characters)");
            }
        }

        out.append('\n');
    }
}

namespace {

struct BMS_AST_Printer {
    Code_String& out;
    const bms::Parsed_Program& program;
    const BMS_AST_Formatting_Options options;

    void print(bms::astp::Handle handle, std::string_view child_name = "", int level = 0)
    {
        BIT_MANIPULATION_ASSERT(level >= 0);
        BIT_MANIPULATION_ASSERT(options.indent_width >= 0);

        out.append(Size(options.indent_width * level), ' ');
        if (handle != bms::astp::Handle::null) {
            const auto handle_int = std::underlying_type_t<bms::astp::Handle>(handle);
            out.append_integer(handle_int, Code_Span_Type::diagnostic_internal);
            out.append(':', Code_Span_Type::diagnostic_punctuation);
        }
        if (child_name != "") {
            out.append(child_name, Code_Span_Type::diagnostic_attribute);
            out.append('=', Code_Span_Type::diagnostic_punctuation);
        }
        if (handle == bms::astp::Handle::null) {
            out.append("null", Code_Span_Type::diagnostic_internal);
            out.append('\n');
            return;
        }

        const bms::astp::Some_Node& node = program.get_node(handle);
        const std::string_view node_name = get_node_name(node);

        out.append(node_name, Code_Span_Type::diagnostic_tag);
        out.append('(', Code_Span_Type::diagnostic_punctuation);
        out.append(program.extract(get_source_position(node)),
                   Code_Span_Type::diagnostic_code_citation);
        out.append(')', Code_Span_Type::diagnostic_punctuation);
        out.append('\n');

        const auto children = get_children(node);
        for (Size i = 0; i < children.size(); ++i) {
            auto name = visit(
                [i](const auto& n) -> std::string_view {
                    if constexpr (requires { n.child_names; }) {
                        return n.child_names[i];
                    }
                    else {
                        return "";
                    }
                },
                node);
            print(children[i], name, level + 1);
        }
    }
};

struct BMD_AST_Printer {
    Code_String& out;
    const bmd::Parsed_Document& program;
    const BMD_AST_Formatting_Options options;

    void print(bmd::ast::Some_Node* node, int level = 0)
    {
        BIT_MANIPULATION_ASSERT(level >= 0);
        BIT_MANIPULATION_ASSERT(options.indent_width >= 0);

        const auto indent = Size(options.indent_width * level);
        out.append(indent, ' ');
        if (node == nullptr) {
            out.append("null", Code_Span_Type::diagnostic_text);
            out.append('\n');
            return;
        }

        const std::string_view node_name = get_node_name(*node);
        const std::string_view extracted = program.extract(get_source_span(*node));

        const auto* const directive = get_if<bmd::ast::Directive>(node);

        if (directive) {
            out.build(Code_Span_Type::diagnostic_tag)
                .append('\\')
                .append(directive->get_identifier());
        }
        else {
            out.append(node_name, Code_Span_Type::diagnostic_attribute);
        }
        out.append('(', Code_Span_Type::diagnostic_punctuation);
        print_cut_off(extracted);
        out.append(')', Code_Span_Type::diagnostic_punctuation);
        out.append('\n');

        if (directive) {
            for (const auto& [key, val] : directive->m_arguments) {
                out.append(indent, ' ');
                out.build(Code_Span_Type::diagnostic_attribute) //
                    .append(key)
                    .append(':');
                out.append(program.extract(get_source_span(val)),
                           Code_Span_Type::diagnostic_code_citation);
                out.append('\n');
            }
        }

        const auto children = get_children(*node);
        for (Size i = 0; i < children.size(); ++i) {
            print(children[i], level + 1);
        }
    }

    /// @brief Prints text which is cut off at some point.
    /// This is useful because the BMD AST often contains nodes with very long text content,
    /// making it impractical to print everything.
    /// @param v the text to print
    void print_cut_off(std::string_view v)
    {
        BIT_MANIPULATION_ASSERT(options.max_node_text_length >= 0);

        int visual_length = 0;

        for (Size i = 0; i < v.length();) {
            if (visual_length >= options.max_node_text_length) {
                out.append("...", Code_Span_Type::diagnostic_punctuation);
                break;
            }

            if (v[i] == '\r') {
                out.append("\\r", Code_Span_Type::diagnostic_escape);
                visual_length += 2;
                ++i;
            }
            else if (v[i] == '\t') {
                out.append("\\t", Code_Span_Type::diagnostic_escape);
                visual_length += 2;
                ++i;
            }
            else if (v[i] == '\n') {
                out.append("\\n", Code_Span_Type::diagnostic_escape);
                visual_length += 2;
                ++i;
            }
            else {
                const auto remainder
                    = v.substr(i, Size(options.max_node_text_length - visual_length));
                const auto part = remainder.substr(0, remainder.find_first_of("\r\t\n"));
                out.append(part, Code_Span_Type::diagnostic_code_citation);
                visual_length += part.size();
                i += part.size();
            }
        }
    }
};

} // namespace

void print_ast(Code_String& out,
               const bms::Parsed_Program& program,
               BMS_AST_Formatting_Options options)
{
    BMS_AST_Printer { out, program, options }.print(program.get_root_handle());
}

void print_ast(Code_String& out,
               const bmd::Parsed_Document& document,
               BMD_AST_Formatting_Options options)
{
    BMD_AST_Printer { out, document, options }.print(document.root_node);
}

void print_internal_error_notice(Code_String& out)
{
    constexpr std::string_view notice = "This is an internal error. Please report this bug at:\n"
                                        "https://github.com/Eisenwave/bit-manipulation/issues\n";
    out.append(notice, Code_Span_Type::diagnostic_internal_error_notice);
}

std::ostream& print_code_string(std::ostream& out, const Code_String& string, bool colors)
{
    const std::string_view text = string.get_text();
    if (!colors) {
        return out << text;
    }

    Code_String_Span previous {};
    for (Code_String_Span span : string) {
        const Size previous_end = previous.begin + previous.length;
        BIT_MANIPULATION_ASSERT(span.begin >= previous_end);
        if (previous_end != span.begin) {
            out << text.substr(previous_end, span.begin - previous_end);
        }
        out << highlight_color_of(span.type) << text.substr(span.begin, span.length) << ansi::reset;
        previous = span;
    }
    const Size last_span_end = previous.begin + previous.length;
    if (last_span_end != text.size()) {
        out << text.substr(last_span_end);
    }

    return out;
}

} // namespace bit_manipulation
