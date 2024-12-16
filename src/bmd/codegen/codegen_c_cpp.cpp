#include <optional>

#include "common/assert.hpp"
#include "common/code_span_type.hpp"
#include "common/code_string.hpp"
#include "common/result.hpp"
#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_value.hpp"
#include "bms/lookup_result.hpp"

#include "bmd/codegen/code_language.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/codegen/generator_base.hpp"

namespace bit_manipulation::bmd {

static_assert(std::ranges::random_access_range<Code_String>);

namespace {

// https://en.cppreference.com/w/cpp/language/operator_precedence
// technically there are more, but the rest can never be produced from BMS
enum struct C_Cpp_Precedence_Group : Default_Underlying {
    conditional,
    logical_or,
    logical_and,
    bitwise_or,
    bitwise_xor,
    bitwise_and,
    equality,
    relational,
    shift,
    additive,
    multiplicative,
    prefix,
    postfix,
    primary
};

std::strong_ordering compare_minimally(C_Cpp_Precedence_Group x, C_Cpp_Precedence_Group y) noexcept
{
    return Default_Underlying(x) <=> Default_Underlying(y);
}

bool is_binary(C_Cpp_Precedence_Group group) noexcept
{
    return Default_Underlying(group) >= Default_Underlying(C_Cpp_Precedence_Group::logical_or)
        && Default_Underlying(group) <= Default_Underlying(C_Cpp_Precedence_Group::multiplicative);
}

bool is_binary_bitwise(C_Cpp_Precedence_Group group) noexcept
{
    const auto underlying = Default_Underlying(group);
    return group == C_Cpp_Precedence_Group::shift
        || (underlying >= Default_Underlying(C_Cpp_Precedence_Group::bitwise_or)
            && underlying <= Default_Underlying(C_Cpp_Precedence_Group::bitwise_and));
}

bool is_binary_logical(C_Cpp_Precedence_Group group) noexcept
{
    return group == C_Cpp_Precedence_Group::logical_and
        || group == C_Cpp_Precedence_Group::logical_or;
}

std::partial_ordering compare_readably(C_Cpp_Precedence_Group x, C_Cpp_Precedence_Group y) noexcept
{
    if (x == y) {
        return std::partial_ordering::equivalent;
    }
    if (is_binary_logical(x) && is_binary_logical(y)) {
        return std::partial_ordering::unordered;
    }
    if ((is_binary_bitwise(x) && is_binary(y)) || (is_binary(x) && is_binary_bitwise(y))) {
        return std::partial_ordering::unordered;
    }
    return compare_minimally(x, y);
}

[[nodiscard]] C_Cpp_Precedence_Group precedence_group_of(bms::Expression_Type type)
{
    using enum bms::Expression_Type;
    switch (type) {
    case if_expression: return C_Cpp_Precedence_Group::conditional;
    case logical_or: return C_Cpp_Precedence_Group::logical_or;
    case logical_and: return C_Cpp_Precedence_Group::logical_and;
    case bitwise_or: return C_Cpp_Precedence_Group::bitwise_or;
    case bitwise_xor: return C_Cpp_Precedence_Group::bitwise_xor;
    case bitwise_and: return C_Cpp_Precedence_Group::bitwise_and;
    case equals:
    case not_equals: return C_Cpp_Precedence_Group::equality;
    case less_than:
    case greater_than:
    case less_or_equal:
    case greater_or_equal: return C_Cpp_Precedence_Group::relational;
    case shift_left:
    case shift_right: return C_Cpp_Precedence_Group::shift;
    case binary_plus:
    case binary_minus: return C_Cpp_Precedence_Group::additive;
    case multiplication:
    case division:
    case remainder: return C_Cpp_Precedence_Group::multiplicative;
    case conversion:
    case unary_plus:
    case unary_minus:
    case logical_not:
    case bitwise_not: return C_Cpp_Precedence_Group::prefix;
    case function_call: return C_Cpp_Precedence_Group::postfix;
    case literal:
    case id: return C_Cpp_Precedence_Group::primary;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid expression type.");
};

enum struct C_Cpp_Dialect : Default_Underlying {
    /// @brief C99
    c99,
    /// @brief C23
    c23,
    /// @brief C++20
    cpp20
};

[[nodiscard]] constexpr bool is_c(C_Cpp_Dialect dialect)
{
    return static_cast<Default_Underlying>(dialect)
        < static_cast<Default_Underlying>(C_Cpp_Dialect::cpp20);
}

static_assert(is_c(C_Cpp_Dialect::c23));

[[nodiscard]] constexpr bool is_cpp(C_Cpp_Dialect dialect)
{
    return dialect == C_Cpp_Dialect::cpp20;
}

enum struct C_Cpp_Type_Type { void_, int_, bool_, uint8, uint16, uint32, uint64, bitint, bituint };

struct C_Cpp_Type {
    C_Cpp_Type_Type type;
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
        out.append_integer(v.int_value, Code_Span_Type::number);
        break;
    case bms::Type_Type::Uint: //
        out.append_integer(Big_Uint(v.int_value), Code_Span_Type::number);
        break;
    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type");
    }
}

void append_type(Code_String& out, C_Cpp_Type type, C_Cpp_Dialect dialect)
{
    const auto prepend_std = [&]() {
        if (is_cpp(dialect)) {
            out.append("std", Code_Span_Type::type_name);
            out.append("::", Code_Span_Type::operation);
        }
    };

    using enum C_Cpp_Type_Type;
    switch (type.type) {
    case void_: //
        out.append("void", Code_Span_Type::keyword);
        return;
    case int_: //
        out.append("int", Code_Span_Type::keyword);
        return;
    case bool_: //
        out.append(dialect == C_Cpp_Dialect::c99 ? "_Bool" : "bool", Code_Span_Type::keyword);
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
        out.append_integer(type.width, Code_Span_Type::number);
        out.append(')', Code_Span_Type::bracket);
        return;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid C type");
}

[[nodiscard]] Result<C_Cpp_Type, Generator_Error_Code>
to_c_type(const bms::Concrete_Type& type, const Code_Options& options, C_Cpp_Dialect dialect)
{
    const bool bitint_allowed = dialect != C_Cpp_Dialect::cpp20 && options.c_23;

    using enum bms::Type_Type;
    switch (type.type()) {
    // TODO: both Nothing and Void correspond to void, but we should also emit _Noreturn
    //       or [[noreturn]] for these languages
    case Nothing:
    case Void: return C_Cpp_Type { C_Cpp_Type_Type::void_ };
    case Int: return C_Cpp_Type { C_Cpp_Type_Type::int_ };
    case Bool: return C_Cpp_Type { C_Cpp_Type_Type::bool_ };

    case Uint:
        int width = type.width();
        if (bitint_allowed && options.c_prefer_bitint) {
            return C_Cpp_Type { C_Cpp_Type_Type::bituint, type.width() };
        }
        switch (width) {
        case 8: return C_Cpp_Type { C_Cpp_Type_Type::uint8 };
        case 16: return C_Cpp_Type { C_Cpp_Type_Type::uint16 };
        case 32: return C_Cpp_Type { C_Cpp_Type_Type::uint32 };
        case 64: return C_Cpp_Type { C_Cpp_Type_Type::uint64 };
        default:
            if (bitint_allowed) {
                return C_Cpp_Type { C_Cpp_Type_Type::bituint, width };
            }
            else {
                return Generator_Error_Code::unsupported_integer_width;
            }
        }
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("BMS types should always have a C equivalent");
}

using namespace bms::ast;

struct C_Cpp_Code_Generator final : Code_Generator_Base {
private:
    const C_Cpp_Dialect m_dialect;

public:
    C_Cpp_Code_Generator(Code_String& out,
                         const bms::Analyzed_Program& program,
                         const Code_Options& options,
                         C_Cpp_Dialect dialect)
        : Code_Generator_Base(out, program, options)
        , m_dialect(dialect)
    {
    }

private:
    [[nodiscard]] Result<void, Generator_Error> generate_code(const Some_Node* node) final;

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

    [[nodiscard]] bool needs_parentheses(bms::Expression_Type outer_type,
                                         const Some_Node& inner) const final
    {
        const C_Cpp_Precedence_Group outer_group = precedence_group_of(outer_type);
        const C_Cpp_Precedence_Group inner_group = precedence_group_of(get_expression_type(inner));
        return !std::is_lteq(compare_readably(outer_group, inner_group));
    }

    struct Visitor;
};

struct C_Cpp_Code_Generator::Visitor {
    C_Cpp_Code_Generator& self;
    const Some_Node* node;

    [[nodiscard]] Result<void, Generator_Error> operator()(const Program& program)
    {
        Scoped_Attempt attempt = self.start_attempt();

        bool first = true;
        for (const Some_Node* declaration : program.get_children()) {
            Scoped_Attempt newline_attempt = self.start_attempt();
            if (!first) {
                self.end_line();
                self.end_line();
            }
            if (auto r = self.generate_code(declaration)) {
                first = false;
                newline_attempt.commit();
            }
            else if (r.error().code != Generator_Error_Code::empty) {
                return r;
            }
        }

        self.end_line();
        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Function& function)
    {
        if (function.is_generic) {
            return Generator_Error { Generator_Error_Code::empty, node };
        }
        Scoped_Attempt attempt = self.start_attempt();

        self.write_indent();
        if (function.get_return_type_node()) {
            if (auto r
                = Visitor { self, function.get_return_type_node() }(function.get_return_type());
                !r) {
                return r;
            }
        }
        else {
            const auto return_type = function.get_concrete_return_type();
            BIT_MANIPULATION_ASSERT(return_type == bms::Concrete_Type::Void);
            self.write_keyword("void");
        }
        self.m_out.append(' ');
        self.m_out.append(function.get_name(), Code_Span_Type::identifier);

        {
            Scoped_Parenthesization p = self.parenthesize();

            bool first = true;
            for (const bms::Parameter& parameter : function.get_parameters()) {
                if (!first) {
                    self.write_separating_comma();
                }
                first = false;
                self.m_out.append(parameter.get_name(), Code_Span_Type::identifier);
                self.m_out.append(':', Code_Span_Type::punctuation);
                self.m_out.append(' ');
                auto v = Visitor { self, parameter.get_type_node() };
                if (auto r = v(parameter.get_type()); !r) {
                    return r;
                }
            }
            if (first) {
                self.write_keyword("void");
            }
        }

        self.separate_after_function();
        if (auto r = Visitor { self, function.get_body_node() }(function.get_body()); !r) {
            return r;
        }

        attempt.commit();
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
        Scoped_Attempt attempt = self.start_attempt();
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
        Scoped_Attempt attempt = self.start_attempt();
        self.write_indent();

        self.m_out.append("if", Code_Span_Type::keyword);
        self.m_out.append(' ');
        {
            Scoped_Parenthesization p = self.parenthesize();
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
        Scoped_Attempt attempt = self.start_attempt();

        self.write_indent();

        self.m_out.append("while", Code_Span_Type::keyword);
        self.m_out.append(' ');

        {
            Scoped_Parenthesization p = self.parenthesize();
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

    [[nodiscard]] Result<void, Generator_Error> operator()(const Control_Statement& statement)
    {
        Scoped_Attempt attempt = self.start_attempt();

        self.write_indent();
        self.write_keyword(control_statement_type_code_name(statement.get_type()));

        if (const Some_Node* expr = statement.get_expression_node()) {
            self.m_out.append(' ');
            if (auto r = self.generate_code(expr); !r) {
                return r;
            }
        }

        self.write_semicolon();

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Assignment& assignment)
    {
        Scoped_Attempt attempt = self.start_attempt();

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
        Scoped_Attempt attempt = self.start_attempt();

        self.m_out.append("{", Code_Span_Type::bracket);
        self.end_line();
        {
            Scoped_Indentation _ = self.push_indent();
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
        constexpr auto outer_type = bms::Expression_Type::conversion;
        Scoped_Attempt attempt = self.start_attempt();
        {
            Scoped_Parenthesization p = self.parenthesize();
            Visitor v { self, conversion.get_target_type_node() };
            if (auto r = v(conversion.get_target_type()); !r) {
                return r;
            }
        }
        if (auto r = self.generate_subexpression(outer_type, conversion.get_expression_node());
            !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const If_Expression& expression)
    {
        constexpr auto outer_type = bms::Expression_Type::if_expression;
        Scoped_Attempt attempt = self.start_attempt();

        const Some_Node& parent = *expression.get_parent();
        const bool parenthesize = is_expression(parent);

        if (parenthesize) {
            self.m_out.append('(', Code_Span_Type::bracket);
        }

        if (auto r = self.generate_subexpression(outer_type, expression.get_condition_node()); !r) {
            return r;
        }
        self.write_infix_operator("?");
        if (auto r = self.generate_subexpression(outer_type, expression.get_left_node()); !r) {
            return r;
        }
        self.write_infix_operator(":");
        if (auto r = self.generate_subexpression(outer_type, expression.get_right_node()); !r) {
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
        const auto outer_type = expression.get_expression_type();
        Scoped_Attempt attempt = self.start_attempt();

        if (auto r = self.generate_subexpression(outer_type, expression.get_left_node()); !r) {
            return r;
        }

        self.write_infix_operator(token_type_code_name(expression.get_op()));

        if (auto r = self.generate_subexpression(outer_type, expression.get_right_node()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Prefix_Expression& expression)
    {
        const auto outer_type = expression.get_expression_type();
        self.m_out.append(token_type_code_name(expression.get_op()), Code_Span_Type::operation);
        return self.generate_subexpression(outer_type, expression.get_expression_node());
    }

    Result<void, Generator_Error> operator()(const Function_Call_Expression& call)
    {
        Scoped_Attempt attempt = self.start_attempt();
        if (call.is_statement()) {
            self.write_indent();
        }

        self.m_out.append(call.get_name(), Code_Span_Type::identifier);
        {
            Scoped_Parenthesization p = self.parenthesize();
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
        if (const auto* const* looked_up_node = get_if<Some_Node*>(&id.lookup_result)) {
            if (const auto* constant = get_if<Const>(*looked_up_node)) {
                // Only C23 supports constexpr; there are no "true constants" prior to that.
                // Therefore, we are forced to inline these whenever used.
                if (!self.m_options.c_23) {
                    append_value(self.m_out, constant->const_value()->concrete_value());
                }
                return {};
            }
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
};

Result<void, Generator_Error> C_Cpp_Code_Generator::generate_code(const Some_Node* node)
{
    return visit(Visitor { *this, node }, *node);
}

Result<void, Generator_Error> generate_c_cpp_code(Code_String& out,
                                                  const bms::Analyzed_Program& program,
                                                  Code_Language language,
                                                  const Code_Options& options)
{
    BIT_MANIPULATION_ASSERT(language == Code_Language::c || language == Code_Language::cpp);
    auto dialect = language == Code_Language::cpp ? C_Cpp_Dialect::cpp20
        : options.c_23                            ? C_Cpp_Dialect::c23
                                                  : C_Cpp_Dialect::c99;
    return C_Cpp_Code_Generator { out, program, options, dialect }();
}

} // namespace

Result<void, Generator_Error>
generate_c_code(Code_String& out, const bms::Analyzed_Program& program, const Code_Options& options)
{
    return generate_c_cpp_code(out, program, Code_Language::c, options);
}

Result<void, Generator_Error> generate_cpp_code(Code_String& out,
                                                const bms::Analyzed_Program& program,
                                                const Code_Options& options)
{
    return generate_c_cpp_code(out, program, Code_Language::cpp, options);
}

} // namespace bit_manipulation::bmd
