#include "common/code_string.hpp"

#include "bms/evaluation/builtin_function.hpp"
#include "bms/expression_type.hpp"
#include "bms/vm/instructions.hpp"

namespace bit_manipulation::bms {

void append_type(Code_String& out, const Concrete_Type& type)
{
    out.append(type_type_name(type.type()), Code_Span_Type::type_name);
    if (type.type() == Type_Type::Uint) {
        out.append('(', Code_Span_Type::punctuation);
        out.append_integer(type.width(), Code_Span_Type::number);
        out.append(')', Code_Span_Type::punctuation);
    }
}

void append_value(Code_String& out, const Concrete_Value& value)
{
    switch (value.type.type()) {
    case Type_Type::Nothing: //
        out.append("Nothing", Code_Span_Type::keyword);
        break;
    case Type_Type::Void: //
        out.append("Nothing", Code_Span_Type::keyword);
        break;
    case Type_Type::Bool: //
        out.append(value.int_value ? "true" : "false", Code_Span_Type::boolean_literal);
        break;
    case Type_Type::Int: //
        out.append_integer(value.int_value, Code_Span_Type::number);
        break;
    case Type_Type::Uint: //
        out.append_integer(Big_Uint(value.int_value), Code_Span_Type::number);
        break;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("value has unknown type.");
}

void append_left_aligned(Code_String& out, std::string_view text, Code_Span_Type type, Size width)
{
    out.append(text, type);
    if (text.size() < width) {
        out.append(width - text.size(), ' ');
    }
}

namespace ins {

constexpr Size name_column_width = 16;

struct Print_Instruction {
    Code_String& out;

    void operator()(const Load& i)
    {
        append_left_aligned(out, "load", Code_Span_Type::keyword, name_column_width);
        if (!i.debug_info.name.empty()) {
            auto comment = out.build(Code_Span_Type::comment);
            comment.append("; ");
            comment.append(i.debug_info.name);
        }
    }

    void operator()(const Store& i)
    {
        append_left_aligned(out, "store", Code_Span_Type::keyword, name_column_width);
        if (!i.debug_info.name.empty()) {
            auto comment = out.build(Code_Span_Type::comment);
            comment.append("; ");
            comment.append(i.debug_info.name);
        }
    }

    void operator()(const Push& i)
    {
        append_left_aligned(out, "push", Code_Span_Type::keyword, name_column_width);
        append_value(out, i.value);
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
        out.append("if", Code_Span_Type::keyword);
        out.append(' ');
        out.append(i.expected ? "true" : "false", Code_Span_Type::boolean_literal);
        out.append(' ');
        out.append("jump", Code_Span_Type::keyword);
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
        out.append(' ');
        append_type(out, i.type);
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
        out.append(' ');
        out.append_integer(i.address, Code_Span_Type::number);
    }

    void operator()(const Builtin_Call& i)
    {
        append_left_aligned(out, "call builtin", Code_Span_Type::keyword, name_column_width);
        out.append(' ');
        out.append(builtin_function_name(i.function), Code_Span_Type::function_name);
    }
};

} // namespace ins

void print_program(Code_String& out, std::span<const Instruction> instructions)
{
    for (const auto& i : instructions) {
        visit(ins::Print_Instruction { out }, i);
        out.append('\n');
    }
}

} // namespace bit_manipulation::bms
