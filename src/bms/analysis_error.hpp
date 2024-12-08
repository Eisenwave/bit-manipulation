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

    /// @brief An unknown annotation was used (e.g. `@gibberish`).
    annotation_unknown,
    /// @brief An annotation cannot be applied to the chosen target (e.g. `@unroll` on a variable).
    annotation_not_applicable,
    /// @brief Too many arguments were provided to an annotation (e.g. `@immutable(1, 2, 3)`).
    annotation_too_many_arguments,
    /// @brief The annotation has no parameter with the specified name (e.g. `@immutable(x = 1)`).
    annotation_unknown_parameter,
    /// @brief The same argument was specified twice (e.g. `@unroll(10, limit=10)`).
    annotation_argument_duplicate,
    /// @brief Mismatch between an annotation parameter type and argument type
    /// (e.g. `@unroll("s")`).
    annotation_argument_wrong_type,
    /// @brief An annotation argument with a bad value was provided (e.g. `@unroll(limit = -1)`).
    annotation_argument_wrong_value,
    /// @brief A required argument was not provided for an annotation (e.g. `@instantiate()`).
    annotation_missing_argument,
};

[[nodiscard]] std::string_view analysis_error_code_name(Analysis_Error_Code code);

struct Annotation_Parameter_Wrong_Argument {
    /// @brief The name of the parameter/argument involved in the error.
    std::string_view name;
    /// @brief The expected (parameter) type.
    Annotation_Parameter_Type expected;
    /// @brief The actual (argument) type.
    Annotation_Parameter_Type actual;
    /// @brief Contains on the value which the parameter has, or an empty string.
    /// This text should be worded so it fits after `must be ...`.
    /// For example, `value_constraints` could be `"positive integer"`.
    std::string_view value_constraints {};
};

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
    Variant<Monostate, Comparison_Failure, Annotation_Parameter_Wrong_Argument> m_extras {};
    /// @brief The node at which the failure took place.
    Debug_Info m_fail {};
    /// @brief The node which is considered to be a cause for the failure,
    /// but not the location of the failure.
    /// For example, if we attempt `x = 0` where `x` is `const`,
    /// the assignment node would be considered the `fail`,
    /// and the constant declaration of `x` would be considered the cause.
    std::optional<Debug_Info> m_cause {};
    std::optional<Value> m_value {};
    std::optional<Concrete_Type> m_type {};
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

    [[nodiscard]] Analysis_Error(Data&& data)
        : Data(std::move(data))
    {
    }

public:
    [[nodiscard]] Analysis_Error_Code code() const
    {
        return m_code.code;
    }

    [[nodiscard]] Evaluation_Error_Code evaluation_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code.code == Analysis_Error_Code::evaluation_error);
        return m_code.evaluation;
    }

    [[nodiscard]] Execution_Error_Code execution_error() const
    {
        BIT_MANIPULATION_ASSERT(m_code.code == Analysis_Error_Code::execution_error);
        return m_code.execution;
    }

    [[nodiscard]] Debug_Info fail() const
    {
        return m_fail;
    }

    [[nodiscard]] std::optional<Debug_Info> cause() const
    {
        return m_cause;
    }

    [[nodiscard]] Construct fail_construct() const
    {
        return m_fail.construct;
    }

    [[nodiscard]] std::optional<Construct> cause_construct() const
    {
        return m_cause ? m_cause->construct : std::optional<Construct> {};
    }

    [[nodiscard]] std::optional<Source_Position> fail_pos() const
    {
        return m_fail.pos;
    }

    [[nodiscard]] std::optional<Source_Position> cause_pos() const
    {
        return m_cause ? m_cause->pos : std::optional<Source_Position> {};
    }

    [[nodiscard]] std::optional<Comparison_Failure> comparison_failure() const
    {
        auto* result = get_if<Comparison_Failure>(&m_extras);
        return result ? *result : std::optional<Comparison_Failure> {};
    }

    [[nodiscard]] std::optional<Annotation_Parameter_Wrong_Argument> wrong_argument() const
    {
        auto* result = get_if<Annotation_Parameter_Wrong_Argument>(&m_extras);
        return result ? *result : std::optional<Annotation_Parameter_Wrong_Argument> {};
    }

    [[nodiscard]] const std::optional<Concrete_Type> type() const
    {
        return m_type;
    }

    [[nodiscard]] const std::optional<Value> value() const
    {
        return m_value;
    }

    friend Builder;
};

struct Analysis_Error_Builder : private detail::Analysis_Error_Data {
private:
    using Data = detail::Analysis_Error_Data;
    bool m_fail_exists = false;

public:
    /// @brief Constructs an error with an `Analysis_Error_Code`.
    /// This constructor should only be used for errors when none of the specialized constructors
    /// below apply.
    /// @param code the error code, which shall be none of `type_error`, `evaluation_error`,
    /// `execution_error`, or `conversion_error`
    [[nodiscard]] explicit Analysis_Error_Builder(Analysis_Error_Code code)
        : Data { .m_code = code }
    {
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::evaluation_error);
        BIT_MANIPULATION_ASSERT(code != bms::Analysis_Error_Code::execution_error);
    }

    [[nodiscard]] explicit Analysis_Error_Builder(Evaluation_Error_Code code)
        : Data { .m_code = code }
    {
    }

    [[nodiscard]] explicit Analysis_Error_Builder(Execution_Error_Code code)
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
    [[nodiscard]] Analysis_Error_Builder(const Execution_Error& error)
        : Data {}

    {
        if (error.code == Execution_Error_Code::evaluation) {
            m_code = Analysis_Error_Code::evaluation_error;
            m_code.evaluation = error.evaluation_error;
        }
        else {
            m_code = Analysis_Error_Code::execution_error;
        }
        if (error.comparison_failure) {
            m_extras = *error.comparison_failure;
        }
    }

    [[nodiscard]] Analysis_Error build()
    {
        BIT_MANIPULATION_ASSERT(m_fail_exists);
        return Analysis_Error { std::move(*this) };
    }

    Analysis_Error_Builder& comparison_failure(const Comparison_Failure& failure)
    {
        m_extras = failure;
        return *this;
    }

    Analysis_Error_Builder& wrong_argument(const Annotation_Parameter_Wrong_Argument& failure)
    {
        m_extras = failure;
        return *this;
    }

    Analysis_Error_Builder& value(const Value& value)
    {
        m_value = value;
        return *this;
    }

    Analysis_Error_Builder& type(const Concrete_Type& type)
    {
        m_type = type;
        return *this;
    }

    Analysis_Error_Builder&
    fail(Construct construct, std::optional<Source_Position> pos = {}, std::string_view name = "")
    {
        m_fail = Debug_Info { construct, pos, name };
        m_fail_exists = true;
        return *this;
    }

    Analysis_Error_Builder& fail(const Debug_Info& info)
    {
        m_fail = info;
        m_fail_exists = true;
        return *this;
    }

    Analysis_Error_Builder& fail(const Parameter& parameter)
    {
        m_fail = debug_info_from_parameter(parameter);
        m_fail_exists = true;
        return *this;
    }

    Analysis_Error_Builder& fail(const ast::Some_Node* node)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        m_fail = Debug_Info { node };
        m_fail_exists = true;
        return *this;
    }

    Analysis_Error_Builder&
    cause(Construct construct, std::optional<Source_Position> pos = {}, std::string_view name = "")
    {
        m_cause = Debug_Info { construct, pos, name };
        return *this;
    }

    Analysis_Error_Builder& cause(const Debug_Info& info)
    {
        m_cause = info;
        return *this;
    }

    Analysis_Error_Builder& cause(const Parameter& parameter)
    {
        m_cause = debug_info_from_parameter(parameter);
        return *this;
    }

    Analysis_Error_Builder& cause(const ast::Some_Node* node)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        m_cause = Debug_Info { node };
        return *this;
    }

    Analysis_Error_Builder& cause(const Lookup_Result& node)
    {
        m_cause = debug_info_from_lookup_result(node);
        return *this;
    }

private:
    [[nodiscard]] static Debug_Info debug_info_from_parameter(const Parameter&);

    [[nodiscard]] static Debug_Info debug_info_from_lookup_result(const Lookup_Result&);
};

} // namespace bit_manipulation::bms

#endif
