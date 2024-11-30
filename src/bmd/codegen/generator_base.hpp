#ifndef BIT_MANIPULATION_BMD_GENERATOR_BASE_HPP
#define BIT_MANIPULATION_BMD_GENERATOR_BASE_HPP

#include "common/result.hpp"

#include "bms/analyzed_program.hpp"

#include "bmd/codegen/code_string.hpp"
#include "bmd/codegen/codegen.hpp"
#include "bmd/codegen/generator_error.hpp"

namespace bit_manipulation::bmd {

struct Scoped_Parenthesization {
private:
    Code_String& out;

public:
    explicit Scoped_Parenthesization(Code_String& out)
        : out(out)
    {
        out.append('(', Code_Span_Type::bracket);
    }

    Scoped_Parenthesization(const Scoped_Parenthesization&) = delete;
    Scoped_Parenthesization& operator=(const Scoped_Parenthesization&) = delete;

    ~Scoped_Parenthesization()
    {
        out.append(')', Code_Span_Type::bracket);
    }
};

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

struct Code_Generator_Base {
protected:
    Code_String& m_out;
    const bms::Analyzed_Program& m_program;
    const Code_Options m_options;
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
        return generate_code(m_program.get_root());
    }

protected:
    [[nodiscard]] virtual Result<void, Generator_Error>
    generate_code(const bms::ast::Some_Node* node) = 0;

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

    Scoped_Indentation push_indent()
    {
        return Scoped_Indentation { m_depth };
    }

    Scoped_Attempt start_attempt()
    {
        return Scoped_Attempt { m_out };
    }

    Scoped_Parenthesization parenthesize()
    {
        return Scoped_Parenthesization { m_out };
    }
};

} // namespace bit_manipulation::bmd

#endif
