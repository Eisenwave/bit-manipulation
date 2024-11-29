#include <optional>

#include "common/assert.hpp"
#include "common/to_string.hpp"
#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_value.hpp"

#include "bmd/code_string.hpp"
#include "bmd/codegen.hpp"

namespace bit_manipulation::bmd {

static_assert(std::ranges::random_access_range<Code_String>);

namespace {

enum struct C_Type_Type { void_, int_, bool_, uint8, uint16, uint32, uint64, bitint, bituint };

struct C_Type {
    C_Type_Type type;
    int width = 0;
};

void append_value(Code_String& out, const bms::Concrete_Value& v)
{
    switch (v.type.type()) {
    case bms::Type_Type::Void: //
        out.append('(', Code_Span_Type::bracket);
        out.append("void", Code_Span_Type::keyword);
        out.append(')', Code_Span_Type::bracket);
        out.append('0', Code_Span_Type::number);
        break;
    case bms::Type_Type::Bool:
        out.append(v.int_value ? "true" : "false", Code_Span_Type::boolean_literal);
        break;
    case bms::Type_Type::Int: //
        out.append(to_string(v.int_value), Code_Span_Type::number);
        break;
    case bms::Type_Type::Uint: //
        out.append(to_string(Big_Uint(v.int_value)), Code_Span_Type::number);
        break;
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type");
    }
}

void append_type(Code_String& out, C_Type type)
{
    using enum C_Type_Type;
    switch (type.type) {
    case void_: //
        out.append("void", Code_Span_Type::keyword);
        break;
    case int_: //
        out.append("int", Code_Span_Type::keyword);
        break;
    case bool_: //
        out.append("_Bool", Code_Span_Type::keyword);
        break;

    case uint8: //
        out.append("uint8_t", Code_Span_Type::type_name);
        break;
    case uint16: //
        out.append("uint16_t", Code_Span_Type::type_name);
        break;
    case uint32: //
        out.append("uint32_t", Code_Span_Type::type_name);
        break;
    case uint64: //
        out.append("uint64_t", Code_Span_Type::type_name);
        break;

    case bituint: //
        out.append("unsigned", Code_Span_Type::keyword);
        out.append(' ');
        [[fallthrough]];
    case bitint:
        out.append("_BitInt", Code_Span_Type::keyword);
        out.append('(', Code_Span_Type::bracket);
        // TODO: using allocations here is really dirty; better avoid
        out.append(std::to_string(type.width));
        out.append(')', Code_Span_Type::bracket);
        break;
    }
}

C_Type to_c_type(const bms::Concrete_Type& type)
{
    using enum bms::Type_Type;
    switch (type.type()) {
    case Void: return { C_Type_Type::void_ };
    case Int: return { C_Type_Type::int_ };
    case Bool: return { C_Type_Type::bool_ };
    case Uint:
        switch (type.width()) {
        case 8: return { C_Type_Type::uint8 };
        case 16: return { C_Type_Type::uint16 };
        case 32: return { C_Type_Type::uint32 };
        case 64: return { C_Type_Type::uint64 };
        default: return { C_Type_Type::bituint, type.width() };
        }
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("BMS types should always have a C equivalent");
}

using namespace bms::ast;

struct Code_Formatting {
    char indent_char = ' ';
    Size indent_size = 4;
};

struct C_Code_Generator {
private:
    Code_String& m_out;
    const bms::Analyzed_Program& m_program;
    Code_Formatting m_formatting {};
    Size m_depth = 0;
    bool m_start_of_line = true;

    struct Indent_Guard {
        Size& indent;

        Indent_Guard(Size& indent)
            : indent(indent)
        {
            ++indent;
        }

        ~Indent_Guard()
        {
            --indent;
        }
    };

    Indent_Guard push_indent()
    {
        return Indent_Guard { m_depth };
    }

public:
    C_Code_Generator(Code_String& out, const bms::Analyzed_Program& program)
        : m_out(out)
        , m_program(program)
    {
    }

    void operator()()
    {
        generate_code(m_program.get_root());
    }

    void operator()(const Program& program)
    {
        for (const Some_Node* node : program.get_children()) {
            generate_code(node);
        }
    }

    void operator()(const Function& function)
    {
        if (function.is_generic) {
            return;
        }
        write_indent();
        (*this)(function.get_return_type());
        m_out.append(' ');
        m_out.append(function.get_name(), Code_Span_Type::identifier);

        m_out.append('(', Code_Span_Type::bracket);

        if (const Some_Node* params_node = function.get_parameters_node()) {
            (*this)(get<Parameter_List>(*params_node));
        }
        else {
            m_out.append("void", Code_Span_Type::keyword);
        }

        m_out.append(')', Code_Span_Type::bracket);
    }

    void operator()(const Parameter_List& parameters)
    {
        const Size n = parameters.get_parameter_count();
        if (n == 0) {
            m_out.append("void", Code_Span_Type::keyword);
            return;
        }

        for (Size i = 0; i < n; ++i) {
            if (i != 0) {
                m_out.append(',', Code_Span_Type::punctuation);
                m_out.append(' ');
            }
            (*this)(parameters.get_parameter(i));
        }
    }

    void operator()(const Parameter& parameter)
    {
        (*this)(parameter.get_type());
        m_out.append(' ');
        m_out.append(parameter.get_name(), Code_Span_Type::identifier);
    }

    void operator()(const Type& type)
    {
        const C_Type c_type = to_c_type(type.concrete_type().value());
        append_type(m_out, c_type);
    }

    void operator()(const Const& constant)
    {
        write_indent();

        m_out.append("constexpr", Code_Span_Type::keyword);
        m_out.append(' ');

        const C_Type c_type = to_c_type(constant.const_value()->get_type());
        append_type(m_out, c_type);
        m_out.append(' ');
        m_out.append(constant.get_name(), Code_Span_Type::identifier);

        m_out.append(' ');
        m_out.append('=', Code_Span_Type::operation);
        m_out.append(' ');
        append_value(m_out, constant.const_value()->concrete_value());

        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Let& variable)
    {
        write_indent();

        const C_Type c_type = to_c_type(variable.const_value()->get_type());
        append_type(m_out, c_type);
        m_out.append(' ');
        m_out.append(variable.get_name(), Code_Span_Type::identifier);

        if (const Some_Node* initializer = variable.get_initializer_node()) {
            m_out.append(' ');
            m_out.append('=', Code_Span_Type::operation);
            m_out.append(' ');
            generate_code(initializer);
        }

        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Static_Assert&) { }

    void operator()(const If_Statement& statement)
    {
        write_indent();

        m_out.append("if", Code_Span_Type::keyword);
        m_out.append(' ');

        m_out.append('(', Code_Span_Type::bracket);
        generate_code(statement.get_condition_node());
        m_out.append(')', Code_Span_Type::bracket);

        m_out.append(' ');
        (*this)(statement.get_if_block());

        if (const Some_Node* else_node = statement.get_else_node()) {
            if (const If_Statement* else_if = get_if<If_Statement>(else_node)) {
                write_indent();
                m_out.append("else", Code_Span_Type::keyword);
                m_out.append(' ');
                (*this)(*else_if);
                return;
            }
            if (const Block_Statement* else_block = get_if<Block_Statement>(else_node)) {
                (*this)(*else_block);
                return;
            }
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected else contents.");
        }
    }

    void operator()(const While_Statement& statement)
    {
        write_indent();

        m_out.append("while", Code_Span_Type::keyword);
        m_out.append(' ');

        m_out.append('(', Code_Span_Type::bracket);
        generate_code(statement.get_condition_node());
        m_out.append(')', Code_Span_Type::bracket);

        m_out.append(' ');
        (*this)(statement.get_block());
    }

    void operator()(const Break&)
    {
        write_indent();
        m_out.append("break", Code_Span_Type::keyword);
        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Continue&)
    {
        write_indent();
        m_out.append("continue", Code_Span_Type::keyword);
        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Return_Statement& statement)
    {
        write_indent();
        m_out.append("return", Code_Span_Type::keyword);

        if (const Some_Node* expr = statement.get_expression_node()) {
            m_out.append(' ');
            generate_code(expr);
        }

        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Assignment& assignment)
    {
        write_indent();
        m_out.append(assignment.get_name(), Code_Span_Type::identifier);
        m_out.append(' ');
        m_out.append('=', Code_Span_Type::operation);
        m_out.append(' ');
        generate_code(assignment.get_expression_node());
        m_out.append(';', Code_Span_Type::punctuation);
        end_line();
    }

    void operator()(const Block_Statement& block)
    {
        write_line("{", Code_Span_Type::bracket);
        {
            Indent_Guard _ = push_indent();
            for (const Some_Node* node : block.get_children()) {
                generate_code(node);
            }
        }
        write_line("}", Code_Span_Type::bracket);
    }

    void operator()(const Conversion_Expression& conversion)
    {
        m_out.append('(', Code_Span_Type::bracket);
        (*this)(conversion.get_target_type());
        m_out.append(')', Code_Span_Type::bracket);
        generate_code(conversion.get_expression_node());
    }

    void operator()(const If_Expression& expression)
    {
        const Some_Node& parent = *expression.get_parent();
        const bool parenthesize = is_expression(parent);

        if (parenthesize) {
            m_out.append('(', Code_Span_Type::bracket);
        }

        generate_code(expression.get_condition_node());
        m_out.append(' ');
        m_out.append('?', Code_Span_Type::operation);
        m_out.append(' ');
        generate_code(expression.get_left_node());
        m_out.append(' ');
        m_out.append(':', Code_Span_Type::operation);
        m_out.append(' ');
        generate_code(expression.get_right_node());

        if (!parenthesize) {
            m_out.append(')', Code_Span_Type::bracket);
        }
    }

    void operator()(const Binary_Expression& expression)
    {
        generate_code(expression.get_left_node());
        m_out.append(' ');
        m_out.append(token_type_code_name(expression.get_op()), Code_Span_Type::operation);
        m_out.append(' ');
        generate_code(expression.get_right_node());
    }

    void operator()(const Prefix_Expression& expression)
    {
        m_out.append(token_type_code_name(expression.get_op()), Code_Span_Type::operation);
        generate_code(expression.get_expression_node());
    }

    void operator()(const Function_Call_Expression& call)
    {
        m_out.append(call.get_name(), Code_Span_Type::identifier);
        m_out.append('(', Code_Span_Type::bracket);
        bool first = true;
        for (const Some_Node* argument : call.get_argument_nodes()) {
            if (!first) {
                m_out.append(',', Code_Span_Type::punctuation);
                m_out.append(' ');
            }
            generate_code(argument);
            first = false;
        }
        m_out.append(')', Code_Span_Type::bracket);
    }

    void operator()(const Id_Expression& id)
    {
        m_out.append(id.get_identifier(), Code_Span_Type::identifier);
    }

    void operator()(const Literal& literal)
    {
        // TODO: preserve original style (hex vs. decimal literal etc.)
        append_value(m_out, literal.const_value()->concrete_value());
    }

    void operator()(const Builtin_Function&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "builtin functions can be looked up, but are not children in the AST");
    }

private:
    void generate_code(const Some_Node* node)
    {
        visit(*this, *node);
    }

    void write_line(std::string_view text)
    {
        BIT_MANIPULATION_ASSERT(m_start_of_line);
        write_indent();
        m_out.append(text);
        end_line();
    }

    void write_line(std::string_view text, Code_Span_Type type)
    {
        BIT_MANIPULATION_ASSERT(m_start_of_line);
        write_indent();
        m_out.append(text, type);
        end_line();
    }

    void write_indent()
    {
        if (m_start_of_line) {
            m_out.append(m_depth * m_formatting.indent_size, m_formatting.indent_char);
            m_start_of_line = false;
        }
    }

    void end_line()
    {
        m_out.append('\n');
        m_start_of_line = true;
    }
};

} // namespace

void generate_code(Code_String& out, const bms::Analyzed_Program& program, Code_Language language)
{
    using enum Code_Language;
    switch (language) {
    case c: return C_Code_Generator { out, program }();
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, codegen only implemented for C.");
    }
}

} // namespace bit_manipulation::bmd
