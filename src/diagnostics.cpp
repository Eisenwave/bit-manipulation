#include <iomanip>
#include <iostream>
#include <span>

#include "diagnostics.hpp"
#include "visit.hpp"

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation {

namespace {

std::string to_string(Uint128 x)
{
    constexpr Uint128 p10_uint64 = 10000000000000000000ULL;

    // TODO: use to_chars if supported for 128-bit
    return x <= std::uint64_t(-1)
        ? std::to_string(static_cast<std::uint64_t>(x))
        : to_string(x / p10_uint64) + std::to_string(static_cast<std::uint64_t>(x % p10_uint64));
}

std::string to_string(Int128 x)
{
    return x >= 0 ? to_string(static_cast<Uint128>(x)) : '-' + to_string(-static_cast<Uint128>(x));
}

std::string to_string(bms::Concrete_Value v)
{
    switch (v.type.type()) {
    case bms::Type_Type::Void: return "Void";
    case bms::Type_Type::Bool: return v.int_value ? "true" : "false";
    case bms::Type_Type::Int: return to_string(v.int_value);
    case bms::Type_Type::Uint: return to_string(static_cast<Big_Uint>(v.int_value));
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type");
    }
}

} // namespace

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

std::string_view to_prose(bms::Analysis_Error_Code e)
{
    using enum bms::Analysis_Error_Code;
    switch (e) {
    case failed_to_define_global_const:
        return "A name was already in use when attempting to define a global constant.";
    case failed_to_define_function:
        return "A name was already in use when attempting to define a function.";
    case failed_to_define_parameter:
        return "A name was already in use when attempting to define a function parameter.";
    case failed_to_define_variable:
        return "A name was already in use when attempting to define a variable.";
    case reference_to_undefined_variable:
        return "An expression attempted to look up a variable or parameter, but it was not "
               "defined.";
    case assignment_of_undefined_variable:
        return "An assignment attempted to assign a variable which is not defined.";
    case call_to_undefined_function:
        return "A function call to an undefined function was attempted.";
    case width_not_integer: return "The width of Uint must be an integer.";
    case width_not_const: return "The width of Uint must be a constant expression.";
    case width_too_large: return "The width of Uint exceeds the maximum.";
    case width_zero: return "The width of Uint must not be zero.";
    case expected_constant_expression:
        return "Expected a constant expression, but was unable to perform constant folding.";
    case let_variable_in_constant_expression:
        return "Cannot use variables in a constant expression. Did you mean 'const'?";
    case parameter_in_constant_expression:
        return "Cannot use function parameters in a constant expression.";
    case function_in_expression:
        return "Attempted to use a function in an expression as if it was a variable.";
    case type_error: return "Type error.";
    case execution_error:
        return "Error in the execution of the generated code for constant-evaluated functions.";
    case evaluation_error: return "Evaluation error in constant expressions or constant folding.";
    case condition_not_bool:
        return "Condition of an if statement or while loop must be of type 'Bool'.";
    case invalid_integer_literal:
        return "The given literal is invalid, possibly because it is too large for the compiler's "
               "internal integer representation.";
    case assigning_parameter: return "Cannot assign a function parameter.";
    case assigning_function: return "Cannot assign a function.";
    case assigning_const: return "Cannot assign a constant. Did you mean to use 'let'?";
    case call_non_function: return "Cannot call something that is not a function.";
    case wrong_number_of_arguments: return "Wrong number of arguments provided to function.";
    case codegen_call_to_unanalyzed:
        return "Constant evaluation depends on a function whose definition is not yet complete.";
    case width_deduction_from_non_uint:
        return "Cannot deduce the width of Uint from a type that is not Uint.";
    case static_assert_expression_not_bool:
        return "The expression of a static assertion must be of type 'Bool'.";
    case static_assertion_failed: return "Static assertion failed.";
    case requires_clause_not_bool:
        return "The expression in a requires-clause must be of type 'Bool.";
    case requires_clause_not_satisfied: return "Requires-clause was not satisfied.";
    case use_of_undefined_variable: return "Use of undefined variable.";
    case use_of_undefined_constant: return "Use of undefined constant.";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view to_prose(bms::Type_Error_Code e)
{
    using enum bms::Type_Error_Code;
    switch (e) {
    case invalid_operator: return "Invalid operator is used.";
    case void_operation: return "Cannot perform operations for operands of type 'Void'.";
    case bool_arithmetic: return "Cannot use arithmetic operators for operands of type 'Bool'.";
    case bool_bitwise: return "Cannot use bitwise operators for operands of type 'Bool'.";
    case bool_relational_comparison:
        return "Cannot use relational comparisons between operands of type 'Bool'. Only logical "
               "comparisons are allowed.";
    case int_bitwise: return "Cannot use bitwise operators for operands of type 'Int'.";
    case uint_logical: return "Cannot us logical operators for operands of type 'Uint'.";
    case non_bool_logical: return "Logical operators can only be applied to 'Bool'.";
    case incompatible_types: return "Incompatible types for operation or conversion.";
    case incompatible_widths: return "Incompatible Uint widths for operation or conversion.";
    case condition_not_bool: return "The condition of an if-expression must be of type 'Bool'";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view to_prose(bms::Evaluation_Error_Code e)
{
    using enum bms::Evaluation_Error_Code;
    switch (e) {
    case type_error: return "The evaluation violates the type system.";
    case int_to_uint_range_error: return "Conversion from 'Int' to 'Uint' loses information.";
    case division_by_zero: return "Division by zero.";
    case shift_too_much: return "Bit-shift was not less than the operand size.";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view to_prose(bms::Execution_Error_Code e)
{
    using enum bms::Execution_Error_Code;
    switch (e) {
    case load_uninitialized: return "Load from uninitialized variable.";
    case pop: return "Pop from empty stack.";
    case pop_call: return "Return from outside a function.";
    case evaluation: return "Error in evaluation of operator.";
    case jump_out_of_program: return "Jump to address which is outside the program.";
    case jump_if_not_bool: return "Condition of Jump_If instruction is not of type 'Bool'.";
    case symbolic_jump: return "Execution of a symbolic jump.";
    case call_out_of_program: return "Call of a function address which is outside the program.";
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid error code");
    }
}

std::string_view cause_to_prose(bms::Analysis_Error_Code e)
{
    using enum bms::Analysis_Error_Code;
    switch (e) {
    case failed_to_define_global_const:
    case failed_to_define_function:
    case failed_to_define_parameter:
    case failed_to_define_variable: return "An entity with the same name is already defined here:";
    case width_not_integer: return "The following expression must be of type 'Int':";
    case width_not_const: return "The following expression is not a constant expression:";
    case width_too_large: return "The following expression exceeded the maximum:";
    case width_zero: return "The following expression evaluated to zero:";
    case let_variable_in_constant_expression:
        return "The referenced variable is declared 'let', here:";
    case parameter_in_constant_expression:
        return "The referenced entity is defined as a function parameter here:";
    case function_in_expression: return "The referenced entity is defined as a function here:";
    case condition_not_bool: return "The following expression must be of type 'Bool':";
    case assigning_parameter: return "The assigned parameter is defined here:";
    case assigning_function: return "The assigned function is defined here:";
    case assigning_const: return "The assigned object is declared 'const' here:";
    case call_non_function: return "The called entity is declared here:";
    case wrong_number_of_arguments: return "The arguments must match the following parameter list:";
    case codegen_call_to_unanalyzed: return "The following function was called:";
    case width_deduction_from_non_uint: return "The following expression is not of type 'Uint':";
    case static_assert_expression_not_bool:
        return "The following expression must be of type 'Bool':";
    case static_assertion_failed:
        return "The following expression must evaluate to 'true', but evaluated to 'false':";
    case requires_clause_not_bool: return "The following expression was not of type 'Bool':";
    case requires_clause_not_satisfied:
        return "The following expression must evaluate to 'true', but evaluated to 'false':";
    case use_of_undefined_variable:
        return "The following variable is undefined before its first use:";
    case use_of_undefined_constant:
        return "The following constant is undefined before its first use:";
    default: return "Caused by:";
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
const std::string note_prefix = std::string(ansi::h_white) + "note: " + std::string(ansi::reset);

std::ostream&
print_affected_line(std::ostream& out, std::string_view source, bms::Source_Position pos)
{
    constexpr std::string_view separator = " | ";

    const std::string_view line = find_line(source, pos.begin);
    return out << std::right << std::setfill(' ') //
               << ansi::h_yellow << std::setw(5) << pos.line + 1 << ansi::reset << separator //
               << line << '\n' //
               << std::setw(5) << "" << separator //
               << std::string(pos.column, ' ') << ansi::h_green << "^\n"
               << ansi::reset;
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
    print_file_position(out, file, error.fail_token.pos) << ": " << error_prefix;
    out << "unexpected token " << token_type_readable_name(error.fail_token.type)
        << " while matching '" << grammar_rule_name(error.fail_rule) << "'\n";

    print_file_position(out, file, error.fail_token.pos) << ": " << note_prefix << "expected ";

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

std::ostream& print_analysis_error(std::ostream& out,
                                   std::string_view file,
                                   const bms::Parsed_Program& program,
                                   bms::Analysis_Error error)
{
    const std::string_view error_prose = error.code == bms::Analysis_Error_Code::type_error
        ? to_prose(error.type_error)
        : to_prose(error.code);

    const auto fail_token = get_token(program.get_node(error.fail));

    print_file_position(out, file, fail_token.pos) << ": " << error_prefix;
    out << error_prose << '\n';
    print_affected_line(out, program.source, fail_token.pos);

    if (error.code == bms::Analysis_Error_Code::execution_error) {
        print_file_position(out, file, fail_token.pos)
            << ": " << note_prefix << to_prose(error.execution_error) << '\n';
    }

    if (error.comparison_failure) {
        const auto cause_token = get_token(program.get_node(error.cause));
        print_file_position(out, file, cause_token.pos) << ": " << note_prefix;
        out << "Comparison evaluated to " //
            << ansi::h_magenta << to_string(error.comparison_failure->left) << ansi::reset //
            << ' ' << token_type_code_name(error.comparison_failure->op) << ' ' //
            << ansi::h_magenta << to_string(error.comparison_failure->right) << ansi::reset << '\n';

        print_affected_line(out, program.source, cause_token.pos);
    }
    else if (error.cause != bms::ast::Handle::null) {
        const auto cause_token = get_token(program.get_node(error.cause));
        print_file_position(out, file, cause_token.pos) << ": " << note_prefix;
        if (error.code == bms::Analysis_Error_Code::evaluation_error) {
            out << "Caused by: " << to_prose(error.evaluation_error) << '\n';
        }
        else {
            out << cause_to_prose(error.code) << '\n';
        }
        print_affected_line(out, program.source, cause_token.pos);
    }

    if (error.code == bms::Analysis_Error_Code::execution_error) {
        print_internal_error_notice(out);
    }

    return out;
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

namespace {

struct AST_Printer {
    std::ostream& out;
    const bms::Parsed_Program& program;
    const Size indent_width;

    void print(bms::ast::Handle handle, std::string_view child_name = "", Size level = 0)
    {
        out << std::string(indent_width * level, ' ');
        if (handle != bms::ast::Handle::null) {
            out << ansi::h_black << static_cast<Size>(handle) << ansi::reset << ':';
        }
        if (child_name != "") {
            out << ansi::h_green << child_name << ansi::black << "=" << ansi::reset;
        }
        if (handle == bms::ast::Handle::null) {
            out << "null\n";
            return;
        }

        const bms::ast::Some_Node node = program.get_node(handle);
        const std::string_view node_name = get_node_name(node);

        out << ansi::h_magenta << node_name << ansi::h_black << "(" << ansi::reset
            << get_token(node).extract(program.source) << ansi::h_black << ")\n"
            << ansi::reset;

        const auto children = get_children(node);
        for (Size i = 0; i < children.size(); ++i) {
            auto name = fast_visit(
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

} // namespace

std::ostream& print_ast(std::ostream& out, const bms::Parsed_Program& program, Size indent_width)
{
    AST_Printer { out, program, indent_width }.print(program.root_node);
    return out;
}

std::ostream& print_internal_error_notice(std::ostream& out)
{
    return out << ansi::h_black
               << "This is an internal compiler error. Please report this bug at:\n"
               << "https://github.com/Eisenwave/bit-manipulation/issues\n"
               << ansi::reset;
}

} // namespace bit_manipulation