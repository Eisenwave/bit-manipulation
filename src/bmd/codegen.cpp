#include <optional>

#include "common/assert.hpp"
#include "common/result.hpp"
#include "common/to_string.hpp"
#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_value.hpp"

#include "bmd/code_string.hpp"
#include "bmd/codegen.hpp"

namespace bit_manipulation::bmd {

static_assert(std::ranges::random_access_range<Code_String>);

[[nodiscard]] std::string_view code_language_name(Code_Language lang)
{
    using enum Code_Language;
    switch (lang) {
        BIT_MANIPULATION_ENUM_STRING_CASE(bms);
        BIT_MANIPULATION_ENUM_STRING_CASE(c);
        BIT_MANIPULATION_ENUM_STRING_CASE(cpp);
        BIT_MANIPULATION_ENUM_STRING_CASE(rust);
        BIT_MANIPULATION_ENUM_STRING_CASE(java);
        BIT_MANIPULATION_ENUM_STRING_CASE(kotlin);
        BIT_MANIPULATION_ENUM_STRING_CASE(javascript);
        BIT_MANIPULATION_ENUM_STRING_CASE(typescript);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid lang");
}

[[nodiscard]] std::string_view code_language_readable_name(Code_Language lang)
{
    using enum Code_Language;
    switch (lang) {
    case bms: return "BMS";
    case c: return "C";
    case cpp: return "C++";
    case rust: return "Rust";
    case java: return "Java";
    case kotlin: return "Kotlin";
    case javascript: return "JavaScript";
    case typescript: return "TypeScript";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid lang");
}

namespace {

enum struct C_Dialect : Default_Underlying {
    /// @brief C99
    c99,
    /// @brief C23
    c23,
    /// @brief C++20
    cpp20
};

[[nodiscard]] constexpr bool is_c(C_Dialect dialect)
{
    return static_cast<Default_Underlying>(dialect)
        < static_cast<Default_Underlying>(C_Dialect::cpp20);
}

[[nodiscard]] constexpr bool is_cpp(C_Dialect dialect)
{
    return dialect == C_Dialect::cpp20;
}

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

void append_type(Code_String& out, C_Type type, C_Dialect dialect)
{
    const auto prepend_std = [&]() {
        if (is_cpp(dialect)) {
            out.append("std", Code_Span_Type::type_name);
            out.append("::", Code_Span_Type::operation);
        }
    };

    using enum C_Type_Type;
    switch (type.type) {
    case void_: //
        out.append("void", Code_Span_Type::keyword);
        return;
    case int_: //
        out.append("int", Code_Span_Type::keyword);
        return;
    case bool_: //
        out.append(dialect == C_Dialect::c99 ? "_Bool" : "bool", Code_Span_Type::keyword);
        return;

    case uint8:
        prepend_std();
        out.append("uint8_t", Code_Span_Type::type_name);
        return;
    case uint16:
        prepend_std();
        out.append("uint16_t", Code_Span_Type::type_name);
        return;
    case uint32:
        prepend_std();
        out.append("uint32_t", Code_Span_Type::type_name);
        return;
    case uint64:
        prepend_std();
        out.append("uint64_t", Code_Span_Type::type_name);
        return;

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
        return;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid C type");
}

[[nodiscard]] Result<C_Type, Generator_Error_Code>
to_c_type(const bms::Concrete_Type& type, const Code_Options& options, C_Dialect dialect)
{
    const bool bitint_allowed = dialect != C_Dialect::cpp20 && options.c_23;

    using enum bms::Type_Type;
    switch (type.type()) {
    case Void: return C_Type { C_Type_Type::void_ };
    case Int: return C_Type { C_Type_Type::int_ };
    case Bool: return C_Type { C_Type_Type::bool_ };

    case Uint:
        int width = type.width();
        if (bitint_allowed && options.c_prefer_bitint) {
            return C_Type { C_Type_Type::bituint, type.width() };
        }
        switch (width) {
        case 8: return C_Type { C_Type_Type::uint8 };
        case 16: return C_Type { C_Type_Type::uint16 };
        case 32: return C_Type { C_Type_Type::uint32 };
        case 64: return C_Type { C_Type_Type::uint64 };
        default:
            if (bitint_allowed) {
                return C_Type { C_Type_Type::bituint, width };
            }
            else {
                return Generator_Error_Code::unsupported_integer_width;
            }
        }
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("BMS types should always have a C equivalent");
}

using namespace bms::ast;

struct C_Code_Generator {
private:
    Code_String& m_out;
    const bms::Analyzed_Program& m_program;
    const Code_Options m_options;
    const C_Dialect m_dialect;
    Size m_depth = 0;
    bool m_start_of_line = true;

public:
    C_Code_Generator(Code_String& out,
                     const bms::Analyzed_Program& program,
                     const Code_Options& options,
                     C_Dialect dialect)
        : m_out(out)
        , m_program(program)
        , m_options(options)
        , m_dialect(dialect)
    {
    }

    bool operator()()
    {
        return generate_code(m_program.get_root()).has_value();
    }

private:
    void separate_after_function()
    {
        if (m_options.break_after_function) {
            end_line();
        }
        else {
            m_out.append(' ');
        }
    }

    void separate_after_if()
    {
        if (m_options.break_after_if) {
            end_line();
        }
        else {
            m_out.append(' ');
        }
    }

    [[nodiscard]] Result<void, Generator_Error> generate_code(const Some_Node* node);

    [[nodiscard]] Result<void, Generator_Error> generate_type(const Some_Node* node,
                                                              const bms::Concrete_Type& type)
    {
        const auto c_type = to_c_type(type, m_options, m_dialect);
        if (!c_type) {
            return Generator_Error { c_type.error(), node };
        }
        append_type(m_out, *c_type, m_dialect);
        return {};
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
            m_out.append(m_depth * m_options.indent_size, m_options.indent_char);
            m_start_of_line = false;
        }
    }

    void write_infix_operator(std::string_view op)
    {
        m_out.append(' ');
        m_out.append(op, Code_Span_Type::operation);
        m_out.append(' ');
    }

    void write_separating_comma()
    {
        m_out.append(',', Code_Span_Type::punctuation);
        m_out.append(' ');
    }

    void end_line()
    {
        m_out.append('\n');
        m_start_of_line = true;
    }

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

    struct Attempt {
    private:
        Code_String* out;
        const Code_String::Length restore_length = out->get_length();

    public:
        Attempt(Code_String& out)
            : out(&out)
        {
        }

        ~Attempt()
        {
            if (out) {
                out->resize(restore_length);
            }
        }

        void commit()
        {
            out = nullptr;
        }
    };

    Attempt start_attempt()
    {
        return Attempt { m_out };
    }

    struct Parenthesization {
    private:
        Code_String& out;

    public:
        Parenthesization(Code_String& out)
            : out(out)
        {
            out.append('(', Code_Span_Type::bracket);
        }

        ~Parenthesization()
        {
            out.append(')', Code_Span_Type::bracket);
        }
    };

    Parenthesization parenthesize()
    {
        return Parenthesization { m_out };
    }

    struct Visitor;
};

struct C_Code_Generator::Visitor {
    C_Code_Generator& self;
    const Some_Node* node;

    [[nodiscard]] Result<void, Generator_Error> operator()(const Program& program)
    {
        Attempt attempt = self.start_attempt();

        bool first = true;
        for (const Some_Node* declaration : program.get_children()) {
            if (auto r = self.generate_code(declaration)) {
                if (!first) {
                    self.end_line();
                }
                first = false;
                self.end_line();
            }
            else if (r.error().code != Generator_Error_Code::empty) {
                return r;
            }
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Function& function)
    {
        if (function.is_generic) {
            return Generator_Error { Generator_Error_Code::empty, node };
        }
        Attempt attempt = self.start_attempt();

        self.write_indent();
        if (auto r = Visitor { self, function.get_return_type_node() }(function.get_return_type());
            !r) {
            return r;
        }
        self.m_out.append(' ');
        self.m_out.append(function.get_name(), Code_Span_Type::identifier);

        {
            Parenthesization p = self.parenthesize();

            if (const Some_Node* params_node = function.get_parameters_node()) {
                Visitor v { self, function.get_parameters_node() };
                if (auto r = v(get<Parameter_List>(*params_node)); !r) {
                    return r;
                }
            }
            else {
                self.m_out.append("void", Code_Span_Type::keyword);
            }
        }

        self.separate_after_function();
        if (auto r = Visitor { self, function.get_body_node() }(function.get_body()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Parameter_List& parameters)
    {
        const Size n = parameters.get_parameter_count();
        if (n == 0) {
            self.m_out.append("void", Code_Span_Type::keyword);
            return {};
        }

        Attempt attempt = self.start_attempt();
        for (Size i = 0; i < n; ++i) {
            if (i != 0) {
                self.write_separating_comma();
            }
            Visitor v { self, parameters.get_parameter_node(i) };
            if (auto r = v(parameters.get_parameter(i)); !r) {
                return r;
            }
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Parameter& parameter)
    {

        if (auto r = Visitor { self, parameter.get_type_node() }(parameter.get_type()); !r) {
            return r;
        }
        self.m_out.append(' ');
        self.m_out.append(parameter.get_name(), Code_Span_Type::identifier);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Type& type)
    {
        return self.generate_type(node, type.concrete_type().value());
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Const& constant)
    {
        if (!self.m_options.c_23) {
            return Generator_Error { Generator_Error_Code::empty, node };
        }
        Attempt attempt = self.start_attempt();
        self.write_indent();

        self.m_out.append("constexpr", Code_Span_Type::keyword);
        self.m_out.append(' ');

        if (auto r = self.generate_type(node, constant.const_value()->get_type()); !r) {
            return r;
        }

        self.m_out.append(' ');
        self.m_out.append(constant.get_name(), Code_Span_Type::identifier);

        self.write_infix_operator("=");
        append_value(self.m_out, constant.const_value()->concrete_value());

        self.m_out.append(';', Code_Span_Type::punctuation);

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Let& variable)
    {
        self.write_indent();

        if (auto r = self.generate_type(node, variable.const_value()->get_type()); !r) {
            return r;
        }

        self.m_out.append(' ');
        self.m_out.append(variable.get_name(), Code_Span_Type::identifier);

        if (const Some_Node* initializer = variable.get_initializer_node()) {
            self.write_infix_operator("=");
            if (auto r = self.generate_code(initializer); !r) {
                return r;
            }
        }

        self.m_out.append(';', Code_Span_Type::punctuation);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Static_Assert&)
    {
        return Generator_Error { Generator_Error_Code::empty, node };
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const If_Statement& statement)
    {
        Attempt attempt = self.start_attempt();
        self.write_indent();

        self.m_out.append("if", Code_Span_Type::keyword);
        self.m_out.append(' ');
        {
            Parenthesization p = self.parenthesize();
            if (auto r = self.generate_code(statement.get_condition_node()); !r) {
                return r;
            }
        }

        self.separate_after_if();
        if (auto r = Visitor { self, statement.get_if_block_node() }(statement.get_if_block());
            !r) {
            return r;
        }

        if (const Some_Node* else_node = statement.get_else_node()) {
            if (const If_Statement* else_if = get_if<If_Statement>(else_node)) {
                self.write_indent();
                self.m_out.append("else", Code_Span_Type::keyword);
                self.m_out.append(' ');
                if (auto r = Visitor { self, else_node }(*else_if); !r) {
                    return r;
                }
            }
            else if (const Block_Statement* else_block = get_if<Block_Statement>(else_node)) {
                if (auto r = Visitor { self, else_node }(*else_block); !r) {
                    return r;
                }
            }
            else {
                BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected else contents.");
            }
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const While_Statement& statement)
    {
        Attempt attempt = self.start_attempt();

        self.write_indent();

        self.m_out.append("while", Code_Span_Type::keyword);
        self.m_out.append(' ');

        {
            Parenthesization p = self.parenthesize();
            if (auto r = self.generate_code(statement.get_condition_node()); !r) {
                return r;
            }
        }

        self.separate_after_if();
        if (auto r = Visitor { self, statement.get_block_node() }(statement.get_block()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Break&)
    {
        self.write_indent();
        self.m_out.append("break", Code_Span_Type::keyword);
        self.m_out.append(';', Code_Span_Type::punctuation);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Continue&)
    {
        self.write_indent();
        self.m_out.append("continue", Code_Span_Type::keyword);
        self.m_out.append(';', Code_Span_Type::punctuation);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Return_Statement& statement)
    {
        Attempt attempt = self.start_attempt();

        self.write_indent();
        self.m_out.append("return", Code_Span_Type::keyword);

        if (const Some_Node* expr = statement.get_expression_node()) {
            self.m_out.append(' ');
            if (auto r = self.generate_code(expr); !r) {
                return r;
            }
        }

        self.m_out.append(';', Code_Span_Type::punctuation);

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Assignment& assignment)
    {
        Attempt attempt = self.start_attempt();

        self.write_indent();
        self.m_out.append(assignment.get_name(), Code_Span_Type::identifier);
        self.write_infix_operator("=");
        if (auto r = self.generate_code(assignment.get_expression_node()); !r) {
            return r;
        }
        self.m_out.append(';', Code_Span_Type::punctuation);

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Block_Statement& block)
    {
        Attempt attempt = self.start_attempt();

        self.m_out.append("{", Code_Span_Type::bracket);
        self.end_line();
        {
            Indent_Guard _ = self.push_indent();
            for (const Some_Node* node : block.get_children()) {
                if (auto r = self.generate_code(node)) {
                    self.end_line();
                }
                else if (r.error().code != Generator_Error_Code::empty) {
                    return r;
                }
            }
        }
        self.write_indent();
        self.m_out.append("}", Code_Span_Type::bracket);

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Conversion_Expression& conversion)
    {
        Attempt attempt = self.start_attempt();
        {
            Parenthesization p = self.parenthesize();
            Visitor v { self, conversion.get_target_type_node() };
            if (auto r = v(conversion.get_target_type()); !r) {
                return r;
            }
        }
        if (auto r = self.generate_code(conversion.get_expression_node()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const If_Expression& expression)
    {
        Attempt attempt = self.start_attempt();

        const Some_Node& parent = *expression.get_parent();
        const bool parenthesize = is_expression(parent);

        if (parenthesize) {
            self.m_out.append('(', Code_Span_Type::bracket);
        }

        if (auto r = self.generate_code(expression.get_condition_node()); !r) {
            return r;
        }
        self.write_infix_operator("?");
        if (auto r = self.generate_code(expression.get_left_node()); !r) {
            return r;
        }
        self.write_infix_operator(":");
        if (auto r = self.generate_code(expression.get_right_node()); !r) {
            return r;
        }

        if (!parenthesize) {
            self.m_out.append(')', Code_Span_Type::bracket);
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Binary_Expression& expression)
    {
        Attempt attempt = self.start_attempt();

        if (auto r = self.generate_code(expression.get_left_node()); !r) {
            return r;
        }

        self.write_infix_operator(token_type_code_name(expression.get_op()));
        if (auto r = self.generate_code(expression.get_right_node()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Prefix_Expression& expression)
    {
        self.m_out.append(token_type_code_name(expression.get_op()), Code_Span_Type::operation);
        return self.generate_code(expression.get_expression_node());
    }

    Result<void, Generator_Error> operator()(const Function_Call_Expression& call)
    {
        Attempt attempt = self.start_attempt();

        self.m_out.append(call.get_name(), Code_Span_Type::identifier);
        {
            Parenthesization p = self.parenthesize();
            bool first = true;
            for (const Some_Node* argument : call.get_argument_nodes()) {
                if (!first) {
                    self.write_separating_comma();
                }
                if (auto r = self.generate_code(argument); !r) {
                    return r;
                }
                first = false;
            }
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Id_Expression& id)
    {
        if (const auto* constant = get_if<Const>(id.lookup_result)) {
            // Only C23 supports constexpr; there are no "true constants" prior to that.
            // Therefore, we are forced to inline these whenever used.
            if (!self.m_options.c_23) {
                append_value(self.m_out, constant->const_value()->concrete_value());
            }
            return {};
        }
        self.m_out.append(id.get_identifier(), Code_Span_Type::identifier);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Literal& literal)
    {
        // TODO: preserve original style (hex vs. decimal literal etc.)
        append_value(self.m_out, literal.const_value()->concrete_value());
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Builtin_Function&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "builtin functions can be looked up, but are not children in the AST");
    }
};

Result<void, Generator_Error> C_Code_Generator::generate_code(const Some_Node* node)
{
    return visit(Visitor { *this, node }, *node);
}

} // namespace

bool generate_code(Code_String& out,
                   const bms::Analyzed_Program& program,
                   Code_Language language,
                   const Code_Options& options)
{
    using enum Code_Language;
    switch (language) {
    case c:
    case cpp: {
        auto dialect = language == Code_Language::cpp ? C_Dialect::cpp20
            : options.c_23                            ? C_Dialect::c23
                                                      : C_Dialect::c99;
        return C_Code_Generator { out, program, options, dialect }();
    }
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, codegen only implemented for C and C++.");
    }
}

} // namespace bit_manipulation::bmd
