#ifndef BIT_MANIPULATION_BMS_ANALYSIS_ERROR_HPP
#define BIT_MANIPULATION_BMS_ANALYSIS_ERROR_HPP

#include <optional>
#include <string_view>

#include "common/source_position.hpp"

#include "bms/comparison_failure.hpp"
#include "bms/evaluation/evaluation_error.hpp"
#include "bms/fwd.hpp"
#include "bms/lookup_result.hpp"
#include "bms/vm/execution_error.hpp"

namespace bit_manipulation::bms {

enum struct Analysis_Error_Code : Default_Underlying {
    /// @brief A name was already in use when attempting to define a global constant.
    failed_to_define_global_const,
    /// @brief A name was already in use when attempting to define a function.
    failed_to_define_function,
    /// @brief A name was already in use when attempting to define a function parameter.
    failed_to_define_parameter,
    /// @brief A name was already in use when attempting to define a variable.
    failed_to_define_variable,
    /// @brief An expression attempted to look up a variable or parameter, but it was not defined.
    reference_to_undefined_variable,
    /// @brief An assignment attempted to assign a variable which is not defined.
    assignment_of_undefined_variable,
    /// @brief A function call to an undefined function was attempted.
    call_to_undefined_function,
    /// @brief The width of an integer is not an integer. (e.g. Uint(true))
    width_not_integer,
    /// @brief The width of an integer exceeds the maximum (`uint_max_width`), is zero, or negative.
    width_invalid,
    /// @brief Expected a constant expression, but was unable to perform constant folding.
    expected_constant_expression,
    /// @brief Attempted to use a variable not declared 'const' in a constant expression.
    let_variable_in_constant_expression,
    /// @brief Attempted to use a function parameter in a constant expression.
    parameter_in_constant_expression,
    /// @brief Attempted to use a function in an expression as if it was a variable.
    function_in_expression,
    /// @brief Error in the execution of the generated code for constexpr functions.
    execution_error,
    /// @brief Evaluation error in constant expressions or constant folding.
    evaluation_error,
    /// @brief Condition of an if statement or while loop is not a `Bool`.
    condition_not_bool,
    /// @brief A given literal is invalid, possibly because it is too large for the compiler's
    /// internal integer representation.
    invalid_integer_literal,
    /// @brief An assignment attempted to assign a function parameter, but function parameters are
    // immutable.
    assigning_parameter,
    /// @brief An attempt was made to assign to a function.
    assigning_function,
    /// @brief An attempt was made to assign to a constant.
    assigning_const,
    /// @brief A function call attempted to call a symbol which is not a function.
    call_non_function,
    /// @brief A function call with the wrong number of arguments was attempted.
    wrong_number_of_arguments,
    /// @brief Constant evaluation depends on a function whose definition is not yet complete.
    codegen_call_to_unanalyzed,
    /// @brief Attempting to deduce the width of `UInt(N)` from a type that is not `Uint`.
    width_deduction_from_non_uint,
    /// @brief The expression in a static assertion is not of type `Bool`.
    static_assert_expression_not_bool,
    /// @brief The expression in a static assertion evaluated to `false`.
    static_assertion_failed,
    /// @brief The expression in a requires-clause is not of type `Bool`.
    requires_clause_not_bool,
    /// @brief The expression in a requires clause evaluated to `false`.
    requires_clause_not_satisfied,
    /// @brief Use of undefined variable.
    use_of_undefined_variable,
    /// @brief Use of undefined constant.
    use_of_undefined_constant,
    /// @brief Empty return statement in non-void function.
    empty_return_in_non_void_function,

    // FIXME: this diagnostic might be dead and should be an assertion instead
    /// @brief Use of an invalid operator.
    invalid_operator,

    /// @brief An operation involving Void was attempted.
    void_operation,
    /// @brief Arithmetic with bool was attempted, such as +true.
    bool_arithmetic,
    /// @brief A bitwise operation with bool was attempted, such as ~false.
    bool_bitwise,
    /// @brief a relational comparison with bool was attempted.
    bool_relational_comparison,
    /// @brief Bitwise operators were applied to Int.
    int_bitwise,

    // FIXME: int_logical and uint_logical may be dead because of non_bool_logical
    /// @brief Logical operators were applied to Int.
    int_logical,
    /// @brief Logical operators were applied to Uint.
    uint_logical,

    /// @brief Logical operators were applied to something other than bool.
    non_bool_logical,
    /// @brief A binary or n-ary operation with incompatible types was attempted.
    incompatible_types,
    /// @brief A binary or n-ary operation with UInts of different widths was attempted.
    incompatible_widths,
    /// @brief Wrong argument type for builtin function call.
    wrong_argument_type,
    /// @brief A break statement appeared outside of a loop.
    break_outside_loop,
    /// @brief A continue statement appeared outside of a loop.
    continue_outside_loop,
};

[[nodiscard]] std::string_view analysis_error_code_name(Analysis_Error_Code code);

namespace detail {

struct Analysis_Error_Detailed_Code {
    Analysis_Error_Code code;
    union {
        Evaluation_Error_Code evaluation;
        Execution_Error_Code execution;
    };

    constexpr Analysis_Error_Detailed_Code() noexcept
        : code {}
        , evaluation {}
    {
    }

    constexpr Analysis_Error_Detailed_Code(Analysis_Error_Code code)
        : code { code }
    {
    }

    constexpr Analysis_Error_Detailed_Code(Evaluation_Error_Code code)
        : code { Analysis_Error_Code::evaluation_error }
        , evaluation { code }
    {
    }

    constexpr Analysis_Error_Detailed_Code(Execution_Error_Code code)
        : code { Analysis_Error_Code::execution_error }
        , execution { code }
    {
    }
};

struct Analysis_Error_Data {
    Analysis_Error_Detailed_Code m_code {};
    /// @brief An optional comparison failure.
    /// This can be added onto errors such as failed `static_assert` or failed calls to `assert`
    /// during constant evaluation, in order to provide detail about the inputs.
    std::optional<Comparison_Failure> m_comparison_failure {};
    /// @brief The node at which the failure took place.
    const ast::Some_Node* m_fail {};
    /// @brief The node which is considered to be a cause for the failure,
    /// but not the location of the failure.
    /// For example, if we attempt `x = 0` where `x` is `const`,
    /// the assignment node would be considered the `fail`,
    /// and the constant declaration of `x` would be considered the cause.
    const ast::Some_Node* m_cause {};
    std::optional<Source_Position> m_fail_pos {};
    std::optional<Source_Position> m_cause_pos {};
};

static_assert(std::is_trivially_copyable_v<Analysis_Error_Data>);

} // namespace detail

/// @brief A high-level error that occurred during program analysis.
/// No matter the cause (name lookup, type checking, execution errors, failed assertions, etc.),
/// everything turns into an `Analysis_Error` at some point.
struct Analysis_Error : private detail::Analysis_Error_Data {
public:
    using Builder = Analysis_Error_Builder;

private:
    using Data = detail::Analysis_Error_Data;

    [[nodiscard]] constexpr Analysis_Error(Data&& data)
        : Data(std::move(data))
    {
    }

public:
    [[nodiscard]] constexpr Analysis_Error_Code code() const
    {
        return m_code.code;
    }

    [[nodiscard]] constexpr Evaluation_Error_Code evaluation_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code.code == Analysis_Error_Code::evaluation_error);
        return m_code.evaluation;
    }

    [[nodiscard]] constexpr Execution_Error_Code execution_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code.code == Analysis_Error_Code::execution_error);
        return m_code.execution;
    }

    [[nodiscard]] constexpr const ast::Some_Node* fail() const
    {
        return m_fail;
    }

    [[nodiscard]] constexpr const ast::Some_Node* cause() const
    {
        return m_cause;
    }

    [[nodiscard]] constexpr std::optional<Source_Position> fail_pos() const
    {
        return m_fail_pos;
    }

    [[nodiscard]] constexpr std::optional<Source_Position> cause_pos() const
    {
        return m_cause_pos;
    }

    [[nodiscard]] constexpr std::optional<Comparison_Failure> comparison_failure() const
    {
        return m_comparison_failure;
    }

    friend Builder;
};

struct Analysis_Error_Builder : private detail::Analysis_Error_Data {
private:
    using Data = detail::Analysis_Error_Data;

public:
    /// @brief Constructs an error with an `Analysis_Error_Code`.
    /// This constructor should only be used for errors when none of the specialized constructors
    /// below apply.
    /// @param code the error code, which shall be none of `type_error`, `evaluation_error`,
    /// `execution_error`, or `conversion_error`
    [[nodiscard]] constexpr explicit Analysis_Error_Builder(Analysis_Error_Code code)
        : Data { .m_code = code }
    {
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::evaluation_error);
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::execution_error);
    }

    [[nodiscard]] constexpr explicit Analysis_Error_Builder(Evaluation_Error_Code code)
        : Data { .m_code = code }
    {
    }

    [[nodiscard]] constexpr explicit Analysis_Error_Builder(Execution_Error_Code code)
        : Data { .m_code = code }
    {
    }

    /// @brief Constructs from an `Execution_Error`.
    /// This correctly treats execution errors that result from evaluation errors as
    /// evaluation errors in analysis.
    ///
    /// For example, if execution runs into `x / 0` (division by zero),
    /// the analysis error is then an `Evaluation_Error_Code::division_by_zero`,
    /// not just some generic `Analysis_Error_Code::execution_error`.
    [[nodiscard]] constexpr Analysis_Error_Builder(const Execution_Error& error)
        : Data { .m_comparison_failure = error.comparison_failure }

    {
        if (error.code == Execution_Error_Code::evaluation) {
            m_code = Analysis_Error_Code::evaluation_error;
            m_code.evaluation = error.evaluation_error;
        }
        else {
            m_code = Analysis_Error_Code::execution_error;
        }
    }

    [[nodiscard]] Analysis_Error build()
    {
        return Analysis_Error { std::move(*this) };
    }

    Analysis_Error_Builder& comparison_failure(const Comparison_Failure& failure)
    {
        m_comparison_failure = failure;
        return *this;
    }

    Analysis_Error_Builder& fail(const Debug_Info&);

    Analysis_Error_Builder& fail(const ast::Some_Node* node);

    Analysis_Error_Builder& fail_pos(const Source_Position& pos)
    {
        m_fail_pos = pos;
        return *this;
    }

    Analysis_Error_Builder& cause(const Debug_Info& node);

    Analysis_Error_Builder& cause(const ast::Some_Node* node);

    Analysis_Error_Builder& cause(const Lookup_Result& node);

    Analysis_Error_Builder& cause_pos(const Source_Position& pos)
    {
        m_cause_pos = pos;
        return *this;
    }
};

} // namespace bit_manipulation::bms

#endif
