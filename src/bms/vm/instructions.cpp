#include "common/code_string.hpp"

#include "bms/ast.hpp"
#include "bms/evaluation/builtin_function.hpp"
#include "bms/expression_type.hpp"
#include "bms/print.hpp"
#include "bms/vm/instructions.hpp"

namespace bit_manipulation::bms {
namespace {

void append_left_aligned(Code_String& out, std::string_view text, Code_Span_Type type, Size width)
{
    out.append(text, type);
    if (text.size() < width) {
        out.append(width - text.size(), ' ');
    }
}

} // namespace

namespace ins {
namespace {

constexpr Size name_column_width = 16;

struct Print_Instruction {
    Code_String& out;
    bool ignore_debug_info = false;

    void operator()(const Load& i)
    {
        append_left_aligned(out, "load", Code_Span_Type::keyword, name_column_width);
        if (!ignore_debug_info && !i.debug_info.name.empty()) {
            auto comment = out.build(Code_Span_Type::comment);
            comment.append("; ");
            comment.append(i.debug_info.name);
        }
    }

    void operator()(const Store& i)
    {
        append_left_aligned(out, "store", Code_Span_Type::keyword, name_column_width);
        if (!ignore_debug_info && !i.debug_info.name.empty()) {
            auto comment = out.build(Code_Span_Type::comment);
            comment.append("; ");
            comment.append(i.debug_info.name);
        }
    }

    void operator()(const Push& i)
    {
        append_left_aligned(out, "push", Code_Span_Type::keyword, name_column_width);
        print_value(out, i.value);
    }

    void operator()(const Pop&)
    {
        out.append("pop", Code_Span_Type::keyword);
    }

    void operator()(const Relative_Jump& i)
    {
        append_left_aligned(out, "jump", Code_Span_Type::keyword, name_column_width);
        out.append_integer(i.offset, Code_Span_Type::number, Sign_Policy::always);
    }

    void operator()(const Relative_Jump_If& i)
    {
        const std::string_view mnemonic = i.expected ? "jump if true" : "jump if false";
        append_left_aligned(out, mnemonic, Code_Span_Type::keyword, name_column_width);
        out.append_integer(i.offset, Code_Span_Type::number, Sign_Policy::always);
    }

    void operator()(const Break&)
    {
        out.append("break", Code_Span_Type::keyword);
    }

    void operator()(const Continue&)
    {
        out.append("continue", Code_Span_Type::keyword);
    }

    void operator()(const Return&)
    {
        out.append("return", Code_Span_Type::keyword);
    }

    void operator()(const Convert& i)
    {
        append_left_aligned(out, "convert to", Code_Span_Type::keyword, name_column_width);
        print_type(out, i.type);
    }

    void operator()(const Unary_Operate& i)
    {
        out.append("unary", Code_Span_Type::keyword);
        out.append(' ');
        out.append(expression_type_code_name(i.op), Code_Span_Type::operation);
    }

    void operator()(const Binary_Operate& i)
    {
        out.append("binary", Code_Span_Type::keyword);
        out.append(' ');
        out.append(expression_type_code_name(i.op), Code_Span_Type::operation);
    }

    void operator()(const Call& i)
    {
        append_left_aligned(out, "call", Code_Span_Type::keyword, name_column_width);
        out.append_integer(i.address, Code_Span_Type::number);
    }

    void operator()(const Symbolic_Call& i)
    {
        append_left_aligned(out, "call", Code_Span_Type::keyword, name_column_width);
        print_function_label(out, *i.target, { .name = true });
    }

    void operator()(const Builtin_Call& i)
    {
        append_left_aligned(out, "call builtin", Code_Span_Type::keyword, name_column_width);
        out.append(builtin_function_name(i.function), Code_Span_Type::function_name);
    }
};

} // namespace
} // namespace ins

void print_program(Code_String& out,
                   std::span<const Instruction> instructions,
                   Program_Print_Options options,
                   Function_Ref<bool(Code_String& out, Size index)> print_label)
{
    for (Size i = 0; i < instructions.size(); ++i) {
        if (print_label && print_label(out, i)) {
            out.append(':', Code_Span_Type::punctuation);
            out.append('\n');
        }
        if (options.indent != 0) {
            out.append(Size(options.indent), ' ');
        }
        visit(ins::Print_Instruction { .out = out, .ignore_debug_info = options.ignore_debug_info },
              instructions[i]);
        out.append('\n');
    }
}

void print_function_label(Code_String& out, const ast::Function& f, Function_Print_Options options)
{
    BIT_MANIPULATION_ASSERT(f.was_analyzed());

    const auto print_space = [&]() {
        if (options.whitespace) {
            out.append(' ');
        }
    };

    if (options.name) {
        out.append(f.get_name(), Code_Span_Type::function_name);
    }

    if (options.parameters) {
        out.append('(', Code_Span_Type::bracket);
        const std::span<const Parameter> parameters = f.get_parameters();
        for (Size i = 0; i < parameters.size(); ++i) {
            if (i != 0) {
                out.append(',', Code_Span_Type::punctuation);
                print_space();
            }
            print_type(out, parameters[i].get_type().concrete_type().value());
        }
        out.append(')', Code_Span_Type::bracket);
    }

    if (options.return_type) {
        print_space();
        out.append("->", Code_Span_Type::punctuation);
        print_space();
        print_type(out, f.get_concrete_return_type());
    }
}

} // namespace bit_manipulation::bms
