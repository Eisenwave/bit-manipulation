#include <optional>

#include "common/assert.hpp"
#include "common/code_span_type.hpp"
#include "common/code_string.hpp"
#include "common/result.hpp"
#include "common/variant.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/concrete_type.hpp"
#include "bms/concrete_value.hpp"
#include "bms/expression_type.hpp"

#include "bmd/code_language.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/codegen/generator_base.hpp"

namespace bit_manipulation::bmd {

static_assert(std::ranges::random_access_range<Code_String>);

namespace {

using namespace bms::ast;

struct Bms_Code_Generator final : Code_Generator_Base {
public:
    Bms_Code_Generator(Code_String& out,
                       const bms::Analyzed_Program& program,
                       const Code_Options& options)
        : Code_Generator_Base(out, program, options)
    {
    }

private:
    [[nodiscard]] Result<void, Generator_Error> generate_code(const Some_Node* node) final;

    void write_operator(bms::Token_Type op)
    {
        Code_Generator_Base::write_operator(bms::token_type_code_name(op));
    }

    void write_keyword(bms::Token_Type keyword)
    {
        Code_Generator_Base::write_keyword(bms::token_type_code_name(keyword));
    }

    void write_infix_operator(bms::Token_Type op, Code_Span_Type type = Code_Span_Type::operation)
    {
        Code_Generator_Base::write_infix_operator(bms::token_type_code_name(op), type);
    }

    void write_infix_keyword(bms::Token_Type op, Code_Span_Type type = Code_Span_Type::keyword)
    {
        Code_Generator_Base::write_infix_keyword(bms::token_type_code_name(op), type);
    }

    [[nodiscard]] bool needs_parentheses(bms::Expression_Type outer_type,
                                         const Some_Node& inner) const final
    {
        const bms::Expression_Type inner_type = bms::ast::get_expression_type(inner);
        return !std::is_lteq(bms::compare_precedence(outer_type, inner_type));
    }

    struct Visitor;
};

struct Bms_Code_Generator::Visitor {
    Bms_Code_Generator& self;
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
        Scoped_Attempt attempt = self.start_attempt();

        self.write_indent();
        self.write_keyword(bms::Token_Type::keyword_function);
        self.m_out.append(' ');
        self.m_out.append(function.get_name(), Code_Span_Type::function_name);

        {
            Scoped_Parenthesization p = self.parenthesize();

            bool first = true;
            for (const bms::Parameter& parameter : function.get_parameters()) {
                if (!first) {
                    self.write_separating_comma();
                }
                first = false;
                self.m_out.append(parameter.get_name(), Code_Span_Type::variable_name);
                self.m_out.append(':', Code_Span_Type::punctuation);
                self.m_out.append(' ');
                auto v = Visitor { self, parameter.get_type_node() };
                if (auto r = v(parameter.get_type()); !r) {
                    return r;
                }
            }
        }
        if (function.get_return_type_node()) {
            self.write_infix_operator(bms::Token_Type::right_arrow, Code_Span_Type::punctuation);
            if (auto r
                = Visitor { self, function.get_return_type_node() }(function.get_return_type());
                !r) {
                return r;
            }
        }
        else if (self.m_options.return_types == Return_Type_Policy::always) {
            self.write_infix_operator(bms::Token_Type::right_arrow, Code_Span_Type::punctuation);
            self.write_keyword(bms::Token_Type::keyword_void);
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
        const bms::Type_Type type_type = type.get_type();
        self.m_out.append(bms::type_type_name(type_type), Code_Span_Type::type_name);

        if (type_type != bms::Type_Type::Uint) {
            return {};
        }

        Scoped_Attempt attempt = self.start_attempt();
        {
            Scoped_Parenthesization p = self.parenthesize();
            BIT_MANIPULATION_ASSERT(type.get_width_node());
            if (self.m_options.always_simplify_widths) {
                const bms::Value& v = get_const_value(*type.get_width_node()).value();
                if (v.is_known()) {
                    self.m_out.append_integer(v.as_uint(), Code_Span_Type::number);
                    attempt.commit();
                    return {};
                }
            }

            if (auto r = self.generate_code(type.get_width_node()); !r) {
                return r;
            }
        }
        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Const& constant)
    {
        Scoped_Attempt attempt = self.start_attempt();
        self.write_indent();

        self.write_keyword(bms::Token_Type::keyword_const);
        self.m_out.append(' ');
        self.m_out.append(constant.get_name(), Code_Span_Type::variable_name);

        if (const Some_Node* type = constant.get_type_node()) {
            self.m_out.append(':', Code_Span_Type::punctuation);
            self.m_out.append(' ');
            if (auto r = Visitor { self, type }(constant.get_type()); !r) {
                return r;
            }
        }

        self.write_infix_operator(bms::Token_Type::assign);
        if (auto r = self.generate_code(constant.get_initializer_node()); !r) {
            return r;
        }

        self.write_semicolon();
        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Let& variable)
    {
        Scoped_Attempt attempt = self.start_attempt();
        self.write_indent();

        self.write_keyword(bms::Token_Type::keyword_let);
        self.m_out.append(' ');
        self.m_out.append(variable.get_name(), Code_Span_Type::variable_name);

        if (const Some_Node* type = variable.get_type_node()) {
            self.m_out.append(':', Code_Span_Type::punctuation);
            self.m_out.append(' ');
            if (auto r = Visitor { self, type }(variable.get_type()); !r) {
                return r;
            }
        }

        if (const Some_Node* initializer = variable.get_initializer_node()) {
            self.write_infix_operator(bms::Token_Type::assign);
            if (auto r = self.generate_code(initializer); !r) {
                return r;
            }
        }
        self.write_semicolon();

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Static_Assert& assertion)
    {
        Scoped_Attempt attempt = self.start_attempt();
        self.write_indent();

        self.write_keyword(bms::Token_Type::keyword_static_assert);
        {
            Scoped_Parenthesization p = self.parenthesize();
            if (auto r = self.generate_code(assertion.get_expression_node()); !r) {
                return r;
            }
        }
        self.write_semicolon();

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const If_Statement& statement)
    {
        Scoped_Attempt attempt = self.start_attempt();
        self.write_indent();

        self.write_keyword(bms::Token_Type::keyword_if);
        self.m_out.append(' ');
        if (auto r = self.generate_code(statement.get_condition_node()); !r) {
            return r;
        }

        self.separate_after_if();
        if (auto r = Visitor { self, statement.get_if_block_node() }(statement.get_if_block());
            !r) {
            return r;
        }

        if (const Some_Node* else_node = statement.get_else_node()) {
            if (const If_Statement* else_if = get_if<If_Statement>(else_node)) {
                self.write_indent();
                self.write_keyword(bms::Token_Type::keyword_else);
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

        self.write_keyword(bms::Token_Type::keyword_while);
        self.m_out.append(' ');

        if (auto r = self.generate_code(statement.get_condition_node()); !r) {
            return r;
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
        self.write_keyword(control_statement_type_token(statement.get_type()));

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
        self.m_out.append(assignment.get_name(), Code_Span_Type::variable_name);
        self.write_infix_operator(bms::Token_Type::assign);
        if (auto r = self.generate_code(assignment.get_expression_node()); !r) {
            return r;
        }
        self.write_semicolon();

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
        if (auto r = self.generate_subexpression(outer_type, conversion.get_expression_node());
            !r) {
            return r;
        }
        self.write_infix_operator(bms::Token_Type::keyword_as, Code_Span_Type::keyword);
        auto v = Visitor { self, conversion.get_target_type_node() };
        if (auto r = v(conversion.get_target_type()); !r) {
            return r;
        }

        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const If_Expression& expression)
    {
        constexpr auto outer_type = bms::Expression_Type::if_expression;
        Scoped_Attempt attempt = self.start_attempt();

        if (auto r = self.generate_subexpression(outer_type, expression.get_left_node()); !r) {
            return r;
        }
        self.write_infix_operator(bms::Token_Type::keyword_if, Code_Span_Type::keyword);
        if (auto r = self.generate_subexpression(outer_type, expression.get_condition_node()); !r) {
            return r;
        }
        self.write_infix_operator(bms::Token_Type::keyword_else, Code_Span_Type::keyword);
        if (auto r = self.generate_subexpression(outer_type, expression.get_right_node()); !r) {
            return r;
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
        self.write_infix_operator(expression.get_op());
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

        self.m_out.append(call.get_name(), Code_Span_Type::function_name);
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

        if (call.is_statement()) {
            self.write_semicolon();
        }
        attempt.commit();
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Id_Expression& id)
    {
        self.m_out.append(id.get_identifier(), Code_Span_Type::variable_name);
        return {};
    }

    [[nodiscard]] Result<void, Generator_Error> operator()(const Literal& literal)
    {
        self.m_out.append(literal.get_literal(), Code_Span_Type::number);
        return {};
    }
};

Result<void, Generator_Error> Bms_Code_Generator::generate_code(const Some_Node* node)
{
    return visit(Visitor { *this, node }, *node);
}

} // namespace

Result<void, Generator_Error> generate_bms_code(Code_String& out,
                                                const bms::Analyzed_Program& program,
                                                std::pmr::memory_resource*,
                                                const Code_Options& options)
{
    return Bms_Code_Generator { out, program, options }();
}

} // namespace bit_manipulation::bmd
