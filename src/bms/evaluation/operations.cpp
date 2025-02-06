#include <algorithm>
#include <ranges>

#include "bms/analysis_error.hpp"
#include "bms/evaluation/builtin_function.hpp"
#include "bms/evaluation/operations.hpp"
#include "bms/expression_type.hpp"

namespace bit_manipulation::bms {

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code> get_common_type(const Concrete_Type& lhs,
                                                                         const Concrete_Type& rhs)
{
    if (lhs == rhs) {
        return lhs;
    }
    if (lhs.type() == Type_Type::Uint && rhs.type() == Type_Type::Uint) {
        return Analysis_Error_Code::incompatible_widths;
    }
    if (lhs.type() == Type_Type::Int && rhs.type() == Type_Type::Uint) {
        return rhs;
    }
    if (lhs.type() == Type_Type::Uint && rhs.type() == Type_Type::Int) {
        return lhs;
    }
    return Analysis_Error_Code::incompatible_types;
}

namespace {

template <typename T>
[[nodiscard]] Result<void, Evaluation_Error_Code> convert_to_equal_type_impl(T& lhs, T& rhs)
{
    const Result<Concrete_Type, Analysis_Error_Code> common
        = get_common_type(lhs.get_type(), rhs.get_type());
    if (!common) {
        return Evaluation_Error_Code::type_error;
    }

    if (auto lhs_result = lhs.convert_to(*common, Conversion_Type::lossless_numeric)) {
        lhs = *lhs_result;
    }
    else {
        return lhs_result.error();
    }

    if (auto rhs_result = rhs.convert_to(*common, Conversion_Type::lossless_numeric)) {
        rhs = *rhs_result;
    }
    else {
        return rhs_result.error();
    }
    return {};
}

[[nodiscard]] Result<void, Evaluation_Error_Code> convert_to_equal_type(Concrete_Value& lhs,
                                                                        Concrete_Value& rhs)
{
    return convert_to_equal_type_impl(lhs, rhs);
}

[[nodiscard]] Result<void, Evaluation_Error_Code> convert_to_equal_type(Value& lhs, Value& rhs)
{
    return convert_to_equal_type_impl(lhs, rhs);
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
result_from_concrete(Result<Concrete_Value, Evaluation_Error_Code> result)
{
    if (result) {
        return Value { result.value() };
    }
    else {
        return result.error();
    }
}

template <std::random_access_iterator Iter>
[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function_impl(Builtin_Function f, Iter begin, Iter end)
{
    static_assert(std::same_as<std::iter_value_t<Iter>, Concrete_Type>);
    if (builtin_parameter_count(f) != Size(end - begin)) {
        return Analysis_Error_Code::wrong_number_of_arguments;
    }
    std::span<const Concrete_Type> parameters = builtin_parameter_types(f);
    BIT_MANIPULATION_ASSERT(parameters.size() == builtin_parameter_count(f));
    for (const Concrete_Type& type : parameters) {
        if (*begin++ != type) {
            return Analysis_Error_Code::wrong_argument_type;
        }
    }
    return builtin_return_type(f);
}

template <std::ranges::random_access_range R>
[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function_impl(Builtin_Function f, const R& args)
{
    return check_builtin_function_impl(f, std::ranges::begin(args), std::ranges::end(args));
}

template <std::random_access_iterator Iter>
[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
unsafe_evaluate_builtin_function_impl(Builtin_Function f, Iter begin, Iter end)
{
    static_assert(std::same_as<std::iter_value_t<Iter>, Concrete_Value>);

    const auto types
        = std::ranges::subrange(begin, end) | std::views::transform(&Concrete_Value::get_type);

    Result<Concrete_Type, Analysis_Error_Code> type_result = check_builtin_function_impl(f, types);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }

    switch (f) {
    case Builtin_Function::assert: {
        if (!begin[0].as_bool()) {
            return Evaluation_Error_Code::assertion_fail;
        }
        return Concrete_Value::Void;
    }
    case Builtin_Function::unreachable: {
        return Evaluation_Error_Code::unreachable;
    }
    }

    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function or fallthrough");
}

template <std::ranges::random_access_range R>
[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
unsafe_evaluate_builtin_function_impl(Builtin_Function f, const R& args)
{
    return unsafe_evaluate_builtin_function_impl(f, std::ranges::begin(args),
                                                 std::ranges::end(args));
}

} // namespace

// TYPE ============================================================================================

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_unary_operator(Expression_Type op, const Concrete_Type& value)
{
    if (!is_prefix_unary(op)) {
        return Analysis_Error_Code::invalid_operator;
    }

    switch (value.type()) {

    case Type_Type::Void: {
        return Analysis_Error_Code::void_operation;
    }

    case Type_Type::Bool: {
        if (is_unary_arithmetic(op)) {
            return Analysis_Error_Code::bool_arithmetic;
        }
        if (op == Expression_Type::bitwise_not) {
            return Analysis_Error_Code::bool_bitwise;
        }
        return value;
    }

    case Type_Type::Int: {
        if (op == Expression_Type::bitwise_not) {
            return Analysis_Error_Code::int_bitwise;
        }
        if (op == Expression_Type::logical_not) {
            return Analysis_Error_Code::int_logical;
        }
        return value;
    }

    case Type_Type::Uint: {
        if (op == Expression_Type::logical_not) {
            return Analysis_Error_Code::uint_logical;
        }
        return value;
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected type.");
    }
}

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_binary_operator(const Concrete_Type& lhs_input,
                      Expression_Type op,
                      const Concrete_Type& rhs_input)
{
    if (!is_binary(op)) {
        return Analysis_Error_Code::invalid_operator;
    }
    if (lhs_input.type() == Type_Type::Void || rhs_input.type() == Type_Type::Void) {
        return Analysis_Error_Code::void_operation;
    }
    Result<Concrete_Type, Analysis_Error_Code> common = get_common_type(lhs_input, rhs_input);
    if (!common) {
        return common.error();
    }
    const Concrete_Type lhs = *common;
    const Concrete_Type rhs = *common;

    switch (lhs.type()) {

    case Type_Type::Void: {
        return Analysis_Error_Code::void_operation;
    }

    case Type_Type::Bool: {
        if (is_binary_arithmetic(op)) {
            return Analysis_Error_Code::bool_arithmetic;
        }
        if (is_binary_bitwise(op)) {
            return Analysis_Error_Code::bool_bitwise;
        }
        if (is_relational_comparison(op)) {
            return Analysis_Error_Code::bool_relational_comparison;
        }
        return Concrete_Type::Bool;
    }

    case Type_Type::Int: {
        if (is_binary_bitwise(op)) {
            return Analysis_Error_Code::int_bitwise;
        }
        if (is_binary_logical(op)) {
            return Analysis_Error_Code::int_logical;
        }
        return is_binary_comparison(op) ? Concrete_Type::Bool : lhs;
    }

    case Type_Type::Uint: {
        if (lhs.width() != rhs.width()) {
            return Analysis_Error_Code::incompatible_widths;
        }
        if (is_binary_logical(op)) {
            return Analysis_Error_Code::uint_logical;
        }
        return is_binary_comparison(op) ? Concrete_Type::Bool : lhs;
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected type.");
    }
}

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_if_expression(const Concrete_Type& lhs,
                    const Concrete_Type& condition,
                    const Concrete_Type& rhs)
{
    if (condition != Concrete_Type::Bool) {
        return Analysis_Error_Code::condition_not_bool;
    }
    Result<Concrete_Type, Analysis_Error_Code> common = get_common_type(lhs, rhs);
    if (!common) {
        return common.error();
    }
    return *common;
}

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Type> args)
{
    return check_builtin_function_impl(f, args);
}

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args)
{
    return check_builtin_function_impl(
        f, args | std::views::transform([](const Concrete_Value& v) { return v.get_type(); }));
}

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Value> args)
{
    return check_builtin_function_impl(
        f, args | std::views::transform([](const Value& v) { return v.get_type(); }));
}

// CONCRETE VALUE ==================================================================================

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_conversion(const Concrete_Value& value, const Concrete_Type& to)
{
    return value.convert_to(to, Conversion_Type::lossless_numeric);
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_unary_operator(Expression_Type op, const Concrete_Value& value)
{
    if (Result<Concrete_Type, Analysis_Error_Code> r = check_unary_operator(op, value.get_type());
        !r) {
        return Evaluation_Error_Code::type_error;
    }

    switch (value.get_type().type()) {

    case Type_Type::Bool: {
        if (op == Expression_Type::logical_not) {
            return Concrete_Value::Bool(value.as_int() ^ 1);
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Bool operation not caught by type-check.");
    }

    case Type_Type::Int: {
        if (op == Expression_Type::unary_plus) {
            return value;
        }
        if (op == Expression_Type::unary_minus) {
            return Concrete_Value::Int(-value.as_int());
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Int operation not caught by type-check.");
    }

    case Type_Type::Uint: {
        if (op == Expression_Type::unary_plus) {
            return value;
        }
        if (op == Expression_Type::unary_minus) {
            return value.transform_uint([](Big_Uint x) { return Big_Uint(-x); });
        }
        if (op == Expression_Type::bitwise_not) {
            return value.transform_uint([](Big_Uint x) { return Big_Uint(~x); });
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Uint operation not caught by type-check.");
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported operation not caught by type-check.");
    }
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_binary_operator(const Concrete_Value& lhs_input,
                         Expression_Type op,
                         const Concrete_Value& rhs_input)
{
    Result<Concrete_Type, Analysis_Error_Code> target_type
        = check_binary_operator(lhs_input.get_type(), op, rhs_input.get_type());
    if (!target_type) {
        return Evaluation_Error_Code::type_error;
    }

    Concrete_Value lhs = lhs_input;
    Concrete_Value rhs = rhs_input;
    if (Result<void, Evaluation_Error_Code> r = convert_to_equal_type(lhs, rhs); !r) {
        return r.error();
    }

    BIT_MANIPULATION_ASSERT(lhs.get_type() == rhs.get_type());

    const auto compare_uint = [&lhs, &rhs](bool f(Big_Uint, Big_Uint)) -> Concrete_Value {
        const Big_Int result = Big_Int(f(lhs.as_uint(), rhs.as_uint()));
        return Concrete_Value { Concrete_Type::Bool, result };
    };

    const auto transform_uint = [&lhs, &rhs](Big_Uint f(Big_Uint, Big_Uint)) -> Concrete_Value {
        Big_Uint result = f(lhs.as_uint(), rhs.as_uint());
        if (lhs.get_type().width() != std::numeric_limits<Big_Uint>::digits) {
            result &= (Big_Uint(1) << rhs.get_type().width()) - 1;
        }
        return Concrete_Value { lhs.get_type(), Big_Int(result) };
    };

    if (lhs.get_type().is_integer()) {
        switch (op) {
        case Expression_Type::division:
        case Expression_Type::remainder: {
            if (rhs.as_int() == 0) {
                return Evaluation_Error_Code::division_by_zero;
            }
            return op == Expression_Type::division
                ? Concrete_Value { lhs.get_type(), lhs.as_int() / rhs.as_int() }
                : Concrete_Value { lhs.get_type(), lhs.as_int() % rhs.as_int() };
        }
        // Relational comparisons don't simply check for bit-equality, so they cannot be handled
        // commonly for both.
        case Expression_Type::equals: //
            return Concrete_Value::Bool(lhs.as_int() == rhs.as_int());
        case Expression_Type::not_equals: //
            return Concrete_Value::Bool(lhs.as_int() != rhs.as_int());

        default: break;
        }
    }

    switch (lhs.get_type().type()) {
        using enum Expression_Type;
    case Type_Type::Bool: {
        if (op == logical_and) {
            return Concrete_Value::Bool(lhs.as_bool() && rhs.as_bool());
        }
        if (op == logical_or) {
            return Concrete_Value::Bool(lhs.as_bool() || rhs.as_bool());
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Bool operation not caught by type-check.");
    }

    case Type_Type::Int: {
        switch (op) {
        case less_than: //
            return Concrete_Value::Bool(lhs.as_int() < rhs.as_int());
        case greater_than: //
            return Concrete_Value::Bool(lhs.as_int() > rhs.as_int());
        case less_or_equal: //
            return Concrete_Value::Bool(lhs.as_int() <= rhs.as_int());
        case greater_or_equal: //
            return Concrete_Value::Bool(lhs.as_int() >= rhs.as_int());
        case binary_plus: //
            return Concrete_Value::Int(lhs.as_int() + rhs.as_int());
        case binary_minus: //
            return Concrete_Value::Int(lhs.as_int() - rhs.as_int());
        case multiplication: //
            return Concrete_Value::Int(lhs.as_int() * rhs.as_int());
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Unsupported Int operation not caught by type-check.");
        }
    }

    case Type_Type::Uint: {
        switch (op) {
        case less_than: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x < y; });
        case greater_than: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x > y; });
        case less_or_equal: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x <= y; });
        case greater_or_equal: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x >= y; });
        case binary_plus: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x + y; });
        case binary_minus: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x - y; });
        case multiplication: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x * y; });
        case bitwise_and: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x & y; });
        case bitwise_or: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x | y; });
        case bitwise_xor: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x ^ y; });
        case shift_left:
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x << y; });
        case shift_right:
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x >> y; });
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Unsupported Uint operation not caught by type-check.");
        }
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported operation not caught by type-check.");
    }
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_if_expression(const Concrete_Value& lhs,
                       const Concrete_Value& condition,
                       const Concrete_Value& rhs)
{
    Result<Concrete_Type, Analysis_Error_Code> type_result
        = check_if_expression(lhs.get_type(), condition.get_type(), rhs.get_type());
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    return (condition.as_bool() ? lhs : rhs)
        .convert_to(*type_result, Conversion_Type::lossless_numeric);
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args)
{
    const auto type_result = check_builtin_function(f, args);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    return unsafe_evaluate_builtin_function_impl(f, args);
}

// VALUE ===========================================================================================

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_conversion(const Value& value,
                                                                       const Concrete_Type& to)
{
    return value.convert_to(to, Conversion_Type::lossless_numeric);
}

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_unary_operator(Expression_Type op,
                                                                           const Value& value)
{
    if (Result<Concrete_Type, Analysis_Error_Code> r = check_unary_operator(op, value.get_type());
        !r) {
        return Evaluation_Error_Code::type_error;
    }
    if (value.is_unknown()) {
        return value;
    }
    return result_from_concrete(evaluate_unary_operator(op, value.concrete_value()));
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_binary_operator(const Value& lhs_input, Expression_Type op, const Value& rhs_input)
{
    const Result<Concrete_Type, Analysis_Error_Code> type_result
        = check_binary_operator(lhs_input.get_type(), op, rhs_input.get_type());
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    Value lhs = lhs_input;
    Value rhs = rhs_input;
    if (Result<void, Evaluation_Error_Code> r = convert_to_equal_type(lhs, rhs); !r) {
        return r.error();
    }
    if (lhs && rhs) {
        return result_from_concrete(
            evaluate_binary_operator(lhs.concrete_value(), op, rhs.concrete_value()));
    }
    // Even if we don't know the values of both operands, there are certain operations which are
    // illegal no matter what the other operand is, such as division by zero.
    if (rhs) {
        using enum Expression_Type;
        if ((op == division || op == remainder) && rhs.as_int() == 0) {
            return Evaluation_Error_Code::division_by_zero;
        }
        if ((op == shift_left || op == shift_right)
            && (rhs.as_int() < 0 || rhs.as_int() >= lhs.get_type().width())) {
            return Evaluation_Error_Code::shift_too_much;
        }
    }

    return Value::unknown_of_type(*type_result);
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_if_expression(const Value& lhs, const Value& condition, const Value& rhs)
{
    Result<Concrete_Type, Analysis_Error_Code> type_result
        = check_if_expression(lhs.get_type(), condition.get_type(), rhs.get_type());
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    if (condition.is_unknown()) {
        return Value::unknown_of_type(*type_result);
    }
    return (condition.as_bool() ? lhs : rhs)
        .convert_to(*type_result, Conversion_Type::lossless_numeric);
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Value> args)
{
    auto type_result = check_builtin_function(f, args);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    if (std::ranges::any_of(args, &Value::is_unknown)) {
        return Value::unknown_of_type(*type_result);
    }
    const auto eval_result = unsafe_evaluate_builtin_function_impl(
        f, args | std::views::transform(&Value::concrete_value));
    if (!eval_result) {
        return eval_result.error();
    }
    return Value { *eval_result };
}

} // namespace bit_manipulation::bms
