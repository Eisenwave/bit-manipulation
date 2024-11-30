#include <iomanip>
#include <iostream>
#include <span>

#include "common/diagnostics.hpp"
#include "common/to_string.hpp"

#include "bms/analysis_error.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_value.hpp"
#include "bms/evaluation_error.hpp"
#include "bms/execution_error.hpp"
#include "bms/fwd.hpp"
#include "bms/operations.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/grammar.hpp"
#include "bms/parsing/parse.hpp"
#include "bms/parsing/parse_error.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/tokenize_error.hpp"

#include "bmd/ast.hpp"
#include "bmd/doc_to_html.hpp"
#include "bmd/grammar.hpp"
#include "bmd/html_writer.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation {

std::string to_string(Uint128 x)
{
    /// The greatest power of 10 that fits into a 64-bit integer.
    constexpr Uint128 exp10_19 = 10000000000000000000ull;

    return x <= std::uint64_t(-1) ? std::to_string(Uint64(x))
                                  : to_string(x / exp10_19) + std::to_string(Uint64(x % exp10_19));
}

std::string to_string(Int128 x)
{
    return x >= 0 ? to_string(Uint128(x)) : '-' + to_string(-Uint128(x));
}

namespace {

std::string value_to_string(bms::Concrete_Value v)
{
    switch (v.type.type()) {
    case bms::Type_Type::Void: return "Void";
    case bms::Type_Type::Bool: return v.int_value ? "true" : "false";
    case bms::Type_Type::Int: return to_string(v.int_value);
    case bms::Type_Type::Uint: return to_string(Big_Uint(v.int_value));
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type");
    }
}

enum struct Error_Line_Type : Default_Underlying { note, error };

struct Printable_Comparison {
    std::string left, right;
    std::string_view op;
};

struct Error_Line {
    Error_Line_Type type;
    std::optional<Source_Position> pos;
    std::string message;
    std::optional<Printable_Comparison> comp {};
};

struct Printable_Error {
    std::string_view source;
    std::pmr::vector<Error_Line> lines {};
    bool is_internal = false;
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

const std::string error_prefix = std::string(ansi::h_red) + "error: " + std::string(ansi::reset);
const std::string note_prefix = std::string(ansi::h_white) + "note: " + std::string(ansi::reset);

std::string_view to_prose(bms::Tokenize_Error_Code e)
{
    switch (e) {
    case bms::Tokenize_Error_Code::illegal_character: //
        return "Illegal character encountered.";
    case bms::Tokenize_Error_Code::integer_suffix: //
        return "Suffix after integer literal is not allowed";
    case bms::Tokenize_Error_Code::unterminated_comment: //
        return "Unterminated block comment found. '/*' must have a matching '*/'";
    default: //
        BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view to_prose(bms::Analysis_Error_Code e)
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
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

std::string_view to_prose(bms::Evaluation_Error_Code e)
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
    case int_to_uint_range_error: //
        return "Conversion from Int to Uint would lose information.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
}

std::string_view to_prose(bms::Execution_Error_Code e)
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

std::string_view to_prose(bmd::Parse_Error_Code e)
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

std::string_view to_prose(bmd::Document_Error_Code e)
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
        return "This attribute cannot have a numeric value; only strings can be provided.";
    case code_tokenization_failure: //
        return "Syntax highlighting for this code snipped failed. Is it ill-formed? Consider using "
               "'text' language or '\\tt' (teletype), not '\\c'.";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid error code.");
}

std::string_view cause_to_prose(bms::Analysis_Error_Code e)
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
    default: //
        return "Caused by:";
    }
}

std::string_view to_prose(IO_Error_Code e)
{
    switch (e) {
    case IO_Error_Code::cannot_open: //
        return "Failed to open file.";
    case IO_Error_Code::read_error: //
        return "I/O error occurred when reading from file.";
    case IO_Error_Code::write_error: //
        return "I/O error occurred when writing to file.";
    default: //
        BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

bool is_incompatible_return_type_error(const bms::Analysis_Error& error)
{
    return error.code() == bms::Analysis_Error_Code::incompatible_types
        && holds_alternative<bms::ast::Return_Statement>(*error.fail);
}

[[nodiscard]] Printable_Error make_error_printable(const bms::Parsed_Program& program,
                                                   const bms::Analysis_Error& error)
{
    Printable_Error result { program.get_source() };

    BIT_MANIPULATION_ASSERT(error.fail);
    const auto fail_pos = get_source_position(*error.fail);
    const auto cause_pos = [&]() -> std::optional<Source_Position> {
        if (error.cause) {
            if (auto result = get_source_position(*error.cause)) {
                return *result;
            }
        }
        return {};
    }();

    if (is_incompatible_return_type_error(error)) {
        BIT_MANIPULATION_ASSERT(error.cause);
        const bool is_void = get<bms::ast::Type>(*error.cause).get_type() == bms::Type_Type::Void;

        result.lines.push_back(
            { Error_Line_Type::error, fail_pos,
              is_void ? "Cannot have non-empty return statement in a function returning Void."
                      : "Invalid conversion between return statement and return type." });
        result.lines.push_back(
            { Error_Line_Type::note, cause_pos,
              is_void ? "Did you mean to 'return;' or declare the return type 'Void'?"
                      : "Return type is declared here:" });
        return result;
    }
    if (error.code() == bms::Analysis_Error_Code::width_invalid) {
        const std::optional<bms::Value>& width_value = get_const_value(*error.fail);
        // if width_invalid got raised during analysis, it means we have to know the width
        BIT_MANIPULATION_ASSERT(width_value);
        const Big_Int width = width_value->as_int();
        std::string prefix = "The width evaluated to " + to_string(width);
        if (width < 0) {
            result.lines.push_back({ Error_Line_Type::error, fail_pos,
                                     std::move(prefix) + ", but widths must be positive." });
        }
        else if (width == 0) {
            result.lines.push_back({ Error_Line_Type::error, fail_pos,
                                     std::move(prefix) + ", but widths shall not be zero." });
        }
        else if (width > uint_max_width) {
            result.lines.push_back({ Error_Line_Type::error, fail_pos,
                                     std::move(prefix) + ", but the maximum allowed is "
                                         + std::to_string(uint_max_width) + '.' });
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("width_invalid raised for seemingly no reason");
        }
        return result;
    }

    const std::string_view error_prose = to_prose(error.code());

    result.lines.push_back({ Error_Line_Type::error, fail_pos, std::string(error_prose) });

    if (error.code() == bms::Analysis_Error_Code::execution_error) {
        result.lines.push_back(
            { Error_Line_Type::note, fail_pos, std::string(to_prose(error.execution_error())) });
    }

    if (error.comparison_failure) {
        result.lines.push_back(
            { Error_Line_Type::note, cause_pos, "Comparison evaluated to ",
              Printable_Comparison { value_to_string(error.comparison_failure->left),
                                     value_to_string(error.comparison_failure->right),
                                     token_type_code_name(error.comparison_failure->op) } });
    }
    else if (error.cause != nullptr) {
        if (error.code() == bms::Analysis_Error_Code::evaluation_error) {
            auto message = "Caused by: " + std::string(to_prose(error.evaluation_error()));
            result.lines.push_back({ Error_Line_Type::note, cause_pos, std::move(message) });
        }
        else {
            result.lines.push_back(
                { Error_Line_Type::note, cause_pos, std::string(cause_to_prose(error.code())) });
        }
    }

    if (error.code() == bms::Analysis_Error_Code::let_variable_in_constant_expression
        || error.code() == bms::Analysis_Error_Code::parameter_in_constant_expression) {
        const auto* type = get_surrounding<bms::ast::Type>(*error.fail);
        if (type) {
            result.lines.push_back({ Error_Line_Type::note, type->get_position(),
                                     "The width of Uint must be a constant expression." });
        }
    }

    result.is_internal = is_internal(error);
    return result;
}

std::ostream&
print_source_position(std::ostream& out, const std::optional<Source_Position>& pos, bool colors)
{
    if (!pos) {
        if (colors) {
            out << ansi::black;
        }
        out << "(internal)";
        if (colors) {
            out << ansi::reset;
        }
        return out;
    }
    return print_file_position(out, pos->file_name, Local_Source_Position { *pos }, colors);
}

std::ostream& print_printable_error(std::ostream& out, const Printable_Error& error, bool colors)
{
    for (const Error_Line& line : error.lines) {
        print_source_position(out, line.pos, colors) << ": ";
        switch (line.type) {
        case Error_Line_Type::error: out << error_prefix; break;
        case Error_Line_Type::note: out << note_prefix; break;
        }
        out << line.message;

        if (line.comp) {
            const auto color = [&](std::string_view c) { return colors ? c : ""; };
            out << color(ansi::h_magenta) << line.comp->left << color(ansi::reset) //
                << ' ' << line.comp->op << ' ' //
                << color(ansi::h_magenta) << line.comp->right << color(ansi::reset);
        }

        out << '\n';
        if (line.pos) {
            print_affected_line(out, error.source, *line.pos, colors);
        }
    }

    if (error.is_internal) {
        print_internal_error_notice(out, colors);
    }

    return out;
}

} // namespace

std::ostream& print_file_position(std::ostream& out,
                                  std::string_view file,
                                  const Local_Source_Position& pos,
                                  bool colors)
{
    if (colors) {
        out << ansi::black;
    }
    out << file << ":" << pos.line + 1 << ":" << pos.column + 1;
    if (colors) {
        out << ansi::reset;
    }
    return out;
}

std::string_view find_line(std::string_view source, Size index)
{
    BIT_MANIPULATION_ASSERT(index < source.size());

    Size begin = source.rfind('\n', index);
    begin = begin != std::string_view::npos ? begin + 1 : 0;

    Size end = std::min(source.find('\n', index + 1), source.size());

    return source.substr(begin, end - begin);
}

std::ostream& print_location_of_file(std::ostream& out, std::string_view file, bool colors)
{
    if (colors) {
        out << ansi::black;
    }
    out << file << ":";
    if (colors) {
        out << ansi::reset;
    }
    return out;
}

std::ostream& print_affected_line(std::ostream& out,
                                  std::string_view source,
                                  const Local_Source_Position& pos,
                                  bool colors)
{
    constexpr std::string_view separator = " | ";

    const std::string_view line = find_line(source, pos.begin);
    if (colors) {
        out << ansi::h_yellow;
    }
    out << std::right << std::setfill(' ') //
        << std::setw(5) << pos.line + 1 << separator;
    if (colors) {
        out << ansi::reset;
    }
    out << line << '\n' //
        << std::setw(5) << "" << separator //
        << std::string(pos.column, ' ');
    if (colors) {
        out << ansi::h_green;
    }
    out << "^\n";
    if (colors) {
        out << ansi::reset;
    }
    return out;
}

std::ostream& print_tokenize_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   const bms::Tokenize_Error& e,
                                   bool colors)
{
    print_file_position(out, file, e.pos, colors);
    out << ": " << error_prefix << to_prose(e.code) << '\n';
    print_affected_line(out, source, e.pos, colors);
    std::exit(1);
    return out;
}

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                const bms::Parse_Error& error,
                                bool colors)
{
    print_file_position(out, file, error.fail_token.pos, colors) << ": " << error_prefix;
    out << "unexpected token " << token_type_readable_name(error.fail_token.type)
        << " while matching '" << grammar_rule_name(error.fail_rule) << "'\n";

    print_file_position(out, file, error.fail_token.pos, colors)
        << ": " << note_prefix << "expected ";

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
    return print_affected_line(out, source, error.fail_token.pos, colors);
}

std::ostream& print_parse_error(std::ostream& out,
                                std::string_view file,
                                std::string_view source,
                                const bmd::Parse_Error& error,
                                bool colors)
{
    print_file_position(out, file, error.pos, colors) << ": " << error_prefix;

    if (error.code == bmd::Parse_Error_Code::unexpected_character) {
        out << "unexpected character '" << source[error.pos.begin] << "' while matching '"
            << grammar_rule_name(error.rule) << "'\n";
        // TODO: diagnose expected character perhaps
    }
    else {
        out << to_prose(error.code) << '\n';
    }

    if (source.empty()) {
        return out;
    }

    return print_affected_line(out, source, error.pos, colors);
}

std::ostream& print_analysis_error(std::ostream& out,
                                   const bms::Parsed_Program& program,
                                   const bms::Analysis_Error& error,
                                   bool colors)
{
    const auto printable = make_error_printable(program, error);
    return print_printable_error(out, printable, colors);
}

std::ostream& print_document_error(std::ostream& out,
                                   std::string_view file,
                                   std::string_view source,
                                   const bmd::Document_Error& error,
                                   bool colors)
{
    print_file_position(out, file, error.pos, colors)
        << ": " << error_prefix << to_prose(error.code) << '\n';

    print_affected_line(out, source, error.pos, colors);
    if (error.code == bmd::Document_Error_Code::writer_misuse) {
        print_internal_error_notice(out, colors);
    }
    return out;
}

std::ostream& print_assertion_error(std::ostream& out, const Assertion_Error& error, bool colors)
{
    if (colors) {
        out << ansi::h_red;
    }
    out << "Assertion failed! ";
    if (colors) {
        out << ansi::reset;
    }

    if (error.type == Assertion_Error_Type::expression) {
        out << "The following expression evaluated to 'false', but was expected to be 'true':\n\n";
    }
    else {
        out << "Code which must be unreachable has been reached.\n\n";
    }

    Local_Source_Position pos { .line = error.location.line(),
                                .column = error.location.column(),
                                .begin = {} };
    print_file_position(out, error.location.file_name(), pos, colors);
    out << ": ";
    if (colors) {
        out << ansi::red;
    }
    out << error.message;
    if (colors) {
        out << ansi::reset;
    }
    out << "\n\n";
    return print_internal_error_notice(out, colors);
}

std::ostream&
print_io_error(std::ostream& out, std::string_view file, IO_Error_Code error, bool colors)
{
    print_location_of_file(std::cout, file, colors) << ": " << to_prose(error) << '\n';
    return out;
}

std::ostream&
print_tokens(std::ostream& out, std::span<const bms::Token> tokens, std::string_view source)
{
    for (const bms::Token& t : tokens) {
        const std::string_view text = source.substr(t.pos.begin, t.pos.length);
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

namespace {

struct BMS_AST_Printer {
    std::ostream& out;
    const bms::Parsed_Program& program;
    const BMS_AST_Formatting_Options options;

    std::string_view color(std::string_view text) const
    {
        return options.colors ? text : "";
    }

    void print(bms::astp::Handle handle, std::string_view child_name = "", int level = 0)
    {
        BIT_MANIPULATION_ASSERT(level >= 0);
        BIT_MANIPULATION_ASSERT(options.indent_width >= 0);

        out << std::string(Size(options.indent_width * level), ' ');
        if (handle != bms::astp::Handle::null) {
            out << color(ansi::h_black) << static_cast<Size>(handle) << color(ansi::reset) << ':';
        }
        if (child_name != "") {
            out << color(ansi::h_green) << child_name << color(ansi::black) << "="
                << color(ansi::reset);
        }
        if (handle == bms::astp::Handle::null) {
            out << "null\n";
            return;
        }

        const bms::astp::Some_Node& node = program.get_node(handle);
        const std::string_view node_name = get_node_name(node);

        out << color(ansi::h_magenta) << node_name //
            << color(ansi::h_black) << "(" //
            << color(ansi::reset) << program.extract(get_source_position(node)) //
            << color(ansi::h_black) << ")\n"
            << color(ansi::reset);

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
    std::ostream& out;
    const bmd::Parsed_Document& program;
    const BMD_AST_Formatting_Options options;

    std::string_view color(std::string_view text) const
    {
        return options.colors ? text : "";
    }

    void print(bmd::ast::Some_Node* node, int level = 0)
    {
        BIT_MANIPULATION_ASSERT(level >= 0);
        BIT_MANIPULATION_ASSERT(options.indent_width >= 0);

        const std::string indent(Size(options.indent_width * level), ' ');
        out << indent;
        if (node == nullptr) {
            out << "null\n";
            return;
        }

        const std::string_view node_name = get_node_name(*node);
        const std::string_view extracted = program.extract(get_source_span(*node));

        const auto* const directive = get_if<bmd::ast::Directive>(node);

        if (directive) {
            out << ansi::h_green << '\\' << directive->get_identifier();
        }
        else {
            out << ansi::h_magenta << node_name;
        }
        out << ansi::h_black << "(" << ansi::reset;
        print_cut_off(out, extracted);
        out << ansi::h_black << ")\n" << ansi::reset;

        if (directive) {
            for (const auto& [key, val] : directive->m_arguments) {
                out << indent << ansi::h_blue << key << ": " << ansi::reset
                    << program.extract(get_source_span(val)) << '\n';
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
    /// @param out the output stream
    /// @param v the text to print
    /// @param max_length the maximum printed length without cutting off by ellipsis
    /// @return `out`
    std::ostream& print_cut_off(std::ostream& out, std::string_view v)
    {
        BIT_MANIPULATION_ASSERT(options.max_node_text_length >= 0);

        int visual_length = 0;

        for (Size i = 0; i < v.length(); ++i) {
            if (visual_length >= options.max_node_text_length) {
                out << color(ansi::h_black) << " ..." << color(ansi::reset);
                break;
            }

            if (v[i] == '\r') {
                out << color(ansi::h_red) << "\\r" << color(ansi::reset);
                visual_length += 2;
            }
            else if (v[i] == '\t') {
                out << color(ansi::h_red) << "\\t" << color(ansi::reset);
                visual_length += 2;
            }
            else if (v[i] == '\n') {
                out << color(ansi::h_red) << "\\n" << color(ansi::reset);
                visual_length += 2;
            }
            else {
                out << v[i];
                visual_length += 1;
            }
        }

        return out;
    }
};

} // namespace

std::ostream&
print_ast(std::ostream& out, const bms::Parsed_Program& program, BMS_AST_Formatting_Options options)
{
    BMS_AST_Printer { out, program, options }.print(program.get_root_handle());
    return out;
}

std::ostream& print_ast(std::ostream& out,
                        const bmd::Parsed_Document& document,
                        BMD_AST_Formatting_Options options)
{
    BMD_AST_Printer { out, document, options }.print(document.root_node);
    return out;
}

std::ostream& print_internal_error_notice(std::ostream& out, bool colors)
{
    if (colors) {
        out << ansi::h_yellow;
    }
    out << "This is an internal error. Please report this bug at:\n"
        << "https://github.com/Eisenwave/bit-manipulation/issues\n";
    if (colors) {
        out << ansi::reset;
    }
    return out;
}

} // namespace bit_manipulation
