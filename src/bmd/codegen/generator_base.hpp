#ifndef BIT_MANIPULATION_BMD_GENERATOR_BASE_HPP
#define BIT_MANIPULATION_BMD_GENERATOR_BASE_HPP

#include "common/code_span_type.hpp"
#include "common/code_string.hpp"
#include "common/result.hpp"

#include "bms/analyzed_program.hpp"

#include "bmd/codegen/codegen.hpp"
#include "bmd/codegen/generator_error.hpp"

namespace bit_manipulation::bmd {

template <char open, char close>
struct Scoped_Brackets {
private:
    Code_String* out;

public:
    explicit Scoped_Brackets(Code_String* out)
        : out(out)
    {
        if (out != nullptr) {
            out->append(open, Code_Span_Type::bracket);
        }
    }

    Scoped_Brackets(const Scoped_Brackets&) = delete;
    Scoped_Brackets& operator=(const Scoped_Brackets&) = delete;

    ~Scoped_Brackets()
    {
        if (out != nullptr) {
            out->append(close, Code_Span_Type::bracket);
        }
    }
};

using Scoped_Parenthesization = Scoped_Brackets<'(', ')'>;
using Scoped_Braces = Scoped_Brackets<'{', '}'>;

struct Scoped_Indentation {
private:
    Size& indent;

public:
    explicit Scoped_Indentation(Size& indent)
        : indent(indent)
    {
        ++indent;
    }

    Scoped_Indentation(const Scoped_Indentation&) = delete;
    Scoped_Indentation& operator=(const Scoped_Indentation&) = delete;

    ~Scoped_Indentation()
    {
        --indent;
    }
};

struct Scoped_Attempt {
private:
    Code_String* out;
    const Code_String::Length restore_length = out->get_length();

public:
    explicit Scoped_Attempt(Code_String& out)
        : out(&out)
    {
    }

    Scoped_Attempt(const Scoped_Attempt&) = delete;
    Scoped_Attempt& operator=(const Scoped_Attempt&) = delete;

    ~Scoped_Attempt()
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

/// @brief Indicates for an AST node whether compactification with adjacent
/// identifiers are possible.
enum struct Compactification : int {
    /// @brief Compactification is never possible, such as for `x`.
    never,
    /// @brief Compactification only before the node is possible, like `a @attr`.
    before,
    /// @brief Compactification only after the node is possible, like `f() as`.
    after,
    /// @brief Compactification is possible both ways, like `if (...) else`.
    always
};

[[nodiscard]] constexpr Compactification operator|(Compactification a, Compactification b)
{
    return Compactification(int(a) | int(b));
}

[[nodiscard]] constexpr bool compactification_is_before(Compactification c)
{
    return c == Compactification::before || c == Compactification::always;
}

[[nodiscard]] constexpr bool compactification_is_after(Compactification c)
{
    return c == Compactification::after || c == Compactification::always;
}

struct Code_Generator_Base {
protected:
    Code_String& m_out;
    const bms::Analyzed_Program& m_program;
    const Code_Options m_options;

private:
    Size m_depth = 0;
    bool m_start_of_line = true;

public:
    Code_Generator_Base(Code_String& out,
                        const bms::Analyzed_Program& program,
                        const Code_Options& options)
        : m_out(out)
        , m_program(program)
        , m_options(options)
    {
    }

    Result<void, Generator_Error> operator()()
    {
        if (auto r = generate_code(m_program.get_root()); !r) {
            return r.error();
        }
        return {};
    }

protected:
    [[nodiscard]] virtual Result<void, Generator_Error>
    generate_code(const bms::ast::Some_Node* node) = 0;

    [[nodiscard]] virtual bool needs_parentheses(bms::Expression_Type outer_type,
                                                 const bms::ast::Some_Node& inner) const
        = 0;

    [[nodiscard]] virtual Compactification can_compactify(const bms::ast::Some_Node& node) const
        = 0;

    [[nodiscard]] bool can_compactify_before(const bms::ast::Some_Node& node) const
    {
        return compactification_is_before(can_compactify(node));
    }

    [[nodiscard]] bool can_compactify_after(const bms::ast::Some_Node& node) const
    {
        return compactification_is_after(can_compactify(node));
    }

    [[nodiscard]] Result<void, Generator_Error>
    generate_subexpression(bms::Expression_Type outer_type, const bms::ast::Some_Node* inner)
    {
        Scoped_Attempt attempt = start_attempt();

        const bool parenthesize
            = m_options.always_parenthesize_subexpressions || needs_parentheses(outer_type, *inner);
        if (parenthesize) {
            m_out.append('(', Code_Span_Type::bracket);
        }

        if (auto r = generate_code(inner); !r) {
            return r;
        }
        if (parenthesize) {
            m_out.append(')', Code_Span_Type::bracket);
        }

        attempt.commit();
        return {};
    }

    void separate_after_function()
    {
        if (!m_options.compactify) {
            if (m_options.break_after_function) {
                end_line();
            }
            else {
                m_out.append(' ');
            }
        }
    }

    void separate_after_if()
    {
        if (!m_options.compactify) {
            if (m_options.break_after_if) {
                end_line();
            }
            else {
                m_out.append(' ');
            }
        }
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
        if (!m_options.compactify && m_start_of_line) {
            m_out.append(m_depth * m_options.indent_size, m_options.indent_char);
            m_start_of_line = false;
        }
    }

    void write_operator(std::string_view op, Code_Span_Type type = Code_Span_Type::operation)
    {
        m_out.append(op, type);
    }

    void write_keyword(std::string_view keyword, Code_Span_Type type = Code_Span_Type::keyword)
    {
        m_out.append(keyword, type);
    }

    void write_number(std::string_view number)
    {
        m_out.append(number, Code_Span_Type::number);
    }

    void write_number(character_convertible auto x)
    {
        m_out.append_integer(x, Code_Span_Type::number);
    }

    void write_variable_name(std::string_view name)
    {
        m_out.append(name, Code_Span_Type::variable_name);
    }

    void write_function_name(std::string_view name)
    {
        m_out.append(name, Code_Span_Type::function_name);
    }

    void write_type_name(std::string_view name)
    {
        m_out.append(name, Code_Span_Type::type_name);
    }

    void write_infix_operator(std::string_view op, Code_Span_Type type = Code_Span_Type::operation)
    {
        write_readability_space();
        m_out.append(op, type);
        write_readability_space();
    }

    void write_infix_keyword(std::string_view op, Code_Span_Type type = Code_Span_Type::keyword)
    {
        write_readability_space();
        m_out.append(op, type);
        write_readability_space();
    }

    void write_semicolon()
    {
        m_out.append(';', Code_Span_Type::punctuation);
    }

    void write_colon()
    {
        m_out.append(':', Code_Span_Type::punctuation);
    }

    void write_separating_comma()
    {
        m_out.append(',', Code_Span_Type::punctuation);
        write_readability_space();
    }

    /// @brief Writes a space which is never mandatory, purely for readability.
    /// For example, this could be introduced in `if (...)`.
    void write_readability_space()
    {
        if (!m_options.compactify) {
            m_out.append(' ');
        }
    }

    /// @brief Writes a space which is always needed.
    /// For example, this could be introduced in `else if`.
    void write_mandatory_space()
    {
        m_out.append(' ');
    }

    void end_line()
    {
        if (!m_options.compactify) {
            m_out.append('\n');
            m_start_of_line = true;
        }
    }

    [[nodiscard]] Scoped_Indentation push_indent()
    {
        return Scoped_Indentation { m_depth };
    }

    [[nodiscard]] Scoped_Attempt start_attempt()
    {
        return Scoped_Attempt { m_out };
    }

    [[nodiscard]] Scoped_Parenthesization parenthesize()
    {
        return Scoped_Parenthesization { &m_out };
    }

    [[nodiscard]] Scoped_Braces in_braces()
    {
        return Scoped_Braces { &m_out };
    }

    [[nodiscard]] Scoped_Parenthesization parenthesize_if(bool condition)
    {
        return Scoped_Parenthesization { condition ? &m_out : nullptr };
    }

    [[nodiscard]] Scoped_Braces in_braces_if(bool condition)
    {
        return Scoped_Braces { condition ? &m_out : nullptr };
    }
};

} // namespace bit_manipulation::bmd

#endif
