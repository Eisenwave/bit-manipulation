#include <algorithm>
#include <ranges>

#include "bms/operations.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

namespace {

[[nodiscard]] bool convert_to_equal_type(Concrete_Type& lhs, Concrete_Type& rhs)
{
    if (lhs == rhs) {
        return true;
    }
    if (lhs.type() == Type_Type::Int && rhs.type() == Type_Type::Uint) {
        lhs = rhs;
        return true;
    }
    else if (lhs.type() == Type_Type::Uint && rhs.type() == Type_Type::Int) {
        rhs = lhs;
        return true;
    }

    return false;
}

template <typename T>
[[nodiscard]] Result<void, Evaluation_Error_Code> convert_to_equal_type_impl(T& lhs, T& rhs)
{
    if (!convert_to_equal_type(lhs.type, rhs.type)) {
        return Evaluation_Error_Code::type_error;
    }

    Concrete_Type lhs_type = lhs.type;
    Concrete_Type rhs_type = rhs.type;

    if (auto [converted, lossy] = lhs.convert_to(lhs_type); !lossy) {
        lhs = converted;
    }
    else {
        return Evaluation_Error_Code::int_to_uint_range_error;
    }

    if (auto [converted, lossy] = lhs.convert_to(rhs_type); !lossy) {
        lhs = converted;
    }
    else {
        return Evaluation_Error_Code::int_to_uint_range_error;
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

template <typename Iter>
[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function_impl(Builtin_Function f, Iter begin, Iter end) noexcept
{
    static_assert(std::random_access_iterator<Iter>);
    static_assert(std::same_as<std::iter_value_t<Iter>, Concrete_Type>);
    if (builtin_parameter_count(f) != Size(end - begin)) {
        return Type_Error_Code::wrong_number_of_arguments;
    }
    switch (f) {
    case Builtin_Function::assert: {
        if (begin[0] != Concrete_Type::Bool) {
            return Type_Error_Code::wrong_argument_type;
        }
        return Concrete_Type::Void;
    }
    }

    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function or fallthrough");
}

template <typename R>
[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function_impl(Builtin_Function f, const R& args) noexcept
{
    static_assert(std::ranges::random_access_range<R>);
    return check_builtin_function_impl(f, std::ranges::begin(args), std::ranges::end(args));
}

template <typename Iter>
[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
unsafe_evaluate_builtin_function_impl(Builtin_Function f, Iter begin, Iter end) noexcept
{
    static_assert(std::random_access_iterator<Iter>);
    static_assert(std::same_as<std::iter_value_t<Iter>, Concrete_Value>);

    const auto types
        = std::ranges::subrange(begin, end) | std::views::transform(&Concrete_Value::type);

    Result<Concrete_Type, Type_Error_Code> type_result = check_builtin_function_impl(f, types);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }

    switch (f) {
    case Builtin_Function::assert: {
        if (begin[0].int_value != 1) {
            return Evaluation_Error_Code::assertion_fail;
        }
        return Concrete_Value::Void;
    }
    }

    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function or fallthrough");
}

template <typename R>
[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
unsafe_evaluate_builtin_function_impl(Builtin_Function f, const R& args) noexcept
{
    static_assert(std::ranges::random_access_range<R>);
    return unsafe_evaluate_builtin_function_impl(f, std::ranges::begin(args),
                                                 std::ranges::end(args));
}

} // namespace

// TYPE ============================================================================================

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_unary_operator(Token_Type op, Concrete_Type value) noexcept
{
    if (!is_unary_operator(op)) {
        return Type_Error_Code::invalid_operator;
    }

    switch (value.type()) {

    case Type_Type::Void: {
        return Type_Error_Code::void_operation;
    }

    case Type_Type::Bool: {
        if (is_arithmetic_operator(op)) {
            return Type_Error_Code::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Type_Error_Code::bool_bitwise;
        }
        return value;
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Type_Error_Code::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Type_Error_Code::int_logical;
        }
        return value;
    }

    case Type_Type::Uint: {
        if (is_logical_operator(op)) {
            return Type_Error_Code::uint_logical;
        }
        return value;
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected type.");
    }
}

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs) noexcept
{
    if (!is_binary_operator(op)) {
        return Type_Error_Code::invalid_operator;
    }
    if (lhs.type() == Type_Type::Void || rhs.type() == Type_Type::Void) {
        return Type_Error_Code::void_operation;
    }
    if (!convert_to_equal_type(lhs, rhs)) {
        return Type_Error_Code::incompatible_types;
    }

    BIT_MANIPULATION_ASSERT(lhs == rhs);

    switch (lhs.type()) {

    case Type_Type::Void: {
        return Type_Error_Code::void_operation;
    }

    case Type_Type::Bool: {
        if (is_arithmetic_operator(op)) {
            return Type_Error_Code::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Type_Error_Code::bool_bitwise;
        }
        if (is_relational_comparison_operator(op)) {
            return Type_Error_Code::bool_relational_comparison;
        }
        return Concrete_Type::Bool;
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Type_Error_Code::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Type_Error_Code::int_logical;
        }
        return is_comparison_operator(op) ? Concrete_Type::Bool : lhs;
    }

    case Type_Type::Uint: {
        if (lhs.width() != rhs.width()) {
            return Type_Error_Code::incompatible_widths;
        }
        if (is_logical_operator(op)) {
            return Type_Error_Code::uint_logical;
        }
        return is_comparison_operator(op) ? Concrete_Type::Bool : lhs;
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected type.");
    }
}

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs) noexcept
{
    if (condition != Concrete_Type::Bool) {
        return Type_Error_Code::condition_not_bool;
    }
    if (!convert_to_equal_type(lhs, rhs)) {
        return Type_Error_Code::incompatible_types;
    }
    BIT_MANIPULATION_ASSERT(lhs == rhs);

    return lhs;
}

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Type> args) noexcept
{
    return check_builtin_function_impl(f, args);
}

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args) noexcept
{
    return check_builtin_function_impl(
        f, args | std::views::transform([](const Concrete_Value& v) { return v.type; }));
}

[[nodiscard]] Result<Concrete_Type, Type_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Value> args) noexcept
{
    return check_builtin_function_impl(
        f, args | std::views::transform([](const Value& v) { return v.type; }));
}

// CONCRETE VALUE ==================================================================================

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_conversion(Concrete_Value value, Concrete_Type to) noexcept
{
    if (!value.type.is_convertible_to(to)) {
        return Evaluation_Error_Code::type_error;
    }

    auto [result, lossy] = value.convert_to(to);
    if (lossy) {
        return Evaluation_Error_Code::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_unary_operator(Token_Type op, Concrete_Value value) noexcept
{
    if (Result<Concrete_Type, Type_Error_Code> r = check_unary_operator(op, value.type); !r) {
        return Evaluation_Error_Code::type_error;
    }

    switch (value.type.type()) {

    case Type_Type::Bool: {
        if (op == Token_Type::logical_not) {
            return Concrete_Value { Concrete_Type::Bool, value.int_value ^ 1 };
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Bool operation not caught by type-check.");
    }

    case Type_Type::Int: {
        if (op == Token_Type::plus) {
            return value;
        }
        if (op == Token_Type::minus) {
            return Concrete_Value { Concrete_Type::Int, -value.int_value };
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Int operation not caught by type-check.");
    }

    case Type_Type::Uint: {
        if (op == Token_Type::plus) {
            return value;
        }
        if (op == Token_Type::minus) {
            return value.transform_uint([](Big_Uint x) { return Big_Uint(-x); });
        }
        if (op == Token_Type::bitwise_not) {
            return value.transform_uint([](Big_Uint x) { return Big_Uint(~x); });
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Uint operation not caught by type-check.");
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported operation not caught by type-check.");
    }
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_binary_operator(Concrete_Value lhs, Token_Type op, Concrete_Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error_Code> target_type
        = check_binary_operator(lhs.type, op, rhs.type);
    if (!target_type) {
        return Evaluation_Error_Code::type_error;
    }

    if (Result<void, Evaluation_Error_Code> r = convert_to_equal_type(lhs, rhs); !r) {
        return r.error();
    }

    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    const auto compare_uint = [&lhs, &rhs](bool f(Big_Uint, Big_Uint)) -> Concrete_Value {
        const Big_Int result = Big_Int(f(Big_Uint(lhs.int_value), Big_Uint(rhs.int_value)));
        return Concrete_Value { Concrete_Type::Bool, result };
    };

    const auto transform_uint = [&lhs, &rhs](Big_Uint f(Big_Uint, Big_Uint)) -> Concrete_Value {
        Big_Uint result = f(Big_Uint(lhs.int_value), Big_Uint(rhs.int_value));
        if (lhs.type.width() != std::numeric_limits<Big_Uint>::digits) {
            result &= (Big_Uint(1) << rhs.type.width()) - 1;
        }
        return Concrete_Value { lhs.type, Big_Int(result) };
    };

    if (lhs.type.is_integer()) {
        switch (op) {
        case Token_Type::division:
        case Token_Type::remainder: {
            if (rhs.int_value == 0) {
                return Evaluation_Error_Code::division_by_zero;
            }
            return op == Token_Type::division
                ? Concrete_Value { lhs.type, lhs.int_value / rhs.int_value }
                : Concrete_Value { lhs.type, lhs.int_value % rhs.int_value };
        }
        // Relational comparisons don't simply check for bit-equality, so they cannot be handled
        // commonly for both.
        case Token_Type::equals: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value == rhs.int_value) };
        case Token_Type::not_equals: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value != rhs.int_value) };
        default: break;
        }
    }

    switch (lhs.type.type()) {
    case Type_Type::Bool: {
        if (op == Token_Type::logical_and) {
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value && rhs.int_value) };
        }
        if (op == Token_Type::logical_or) {
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value || rhs.int_value) };
        }
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unsupported Bool operation not caught by type-check.");
    }

    case Type_Type::Int: {
        switch (op) {
        case Token_Type::less_than: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value < rhs.int_value) };
        case Token_Type::greater_than: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value > rhs.int_value) };
        case Token_Type::less_or_equal: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value <= rhs.int_value) };
        case Token_Type::greater_or_equal: //
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value >= rhs.int_value) };
        case Token_Type::plus: //
            return Concrete_Value { Concrete_Type::Int, Big_Int(lhs.int_value + rhs.int_value) };
        case Token_Type::minus: //
            return Concrete_Value { Concrete_Type::Int, Big_Int(lhs.int_value - rhs.int_value) };
        case Token_Type::multiplication: //
            return Concrete_Value { Concrete_Type::Int, Big_Int(lhs.int_value * rhs.int_value) };
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Unsupported Int operation not caught by type-check.");
        }
    }

    case Type_Type::Uint: {

        switch (op) {
        case Token_Type::less_than: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x < y; });
        case Token_Type::greater_than: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x > y; });
        case Token_Type::less_or_equal: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x <= y; });
        case Token_Type::greater_or_equal: //
            return compare_uint([](Big_Uint x, Big_Uint y) { return x >= y; });
        case Token_Type::plus: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x + y; });
        case Token_Type::minus: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x - y; });
        case Token_Type::multiplication: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x * y; });
        case Token_Type::bitwise_and: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x & y; });
        case Token_Type::bitwise_or: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x | y; });
        case Token_Type::bitwise_xor: //
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x ^ y; });
        case Token_Type::shift_left:
            return transform_uint([](Big_Uint x, Big_Uint y) -> Big_Uint { return x << y; });
        case Token_Type::shift_right:
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
evaluate_if_expression(Concrete_Value lhs, Concrete_Value condition, Concrete_Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error_Code> type_result
        = check_if_expression(lhs.type, condition.type, rhs.type);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    const auto [result, lossy] = (condition.int_value ? lhs : rhs).convert_to(*type_result);
    if (lossy) {
        return Evaluation_Error_Code::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args) noexcept
{
    auto type_result = check_builtin_function(f, args);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    return unsafe_evaluate_builtin_function_impl(f, args);
}

// VALUE ===========================================================================================

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_conversion(Value value,
                                                                       Concrete_Type to) noexcept
{
    if (!value.type.is_convertible_to(to)) {
        return Evaluation_Error_Code::type_error;
    }

    auto [result, lossy] = value.convert_to(to);
    if (lossy) {
        return Evaluation_Error_Code::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_unary_operator(Token_Type op,
                                                                           Value value) noexcept
{
    if (Result<Concrete_Type, Type_Error_Code> r = check_unary_operator(op, value.type); !r) {
        return Evaluation_Error_Code::type_error;
    }
    if (value.is_unknown()) {
        return value;
    }
    return result_from_concrete(evaluate_unary_operator(op, value.concrete_value()));
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept
{
    const Result<Concrete_Type, Type_Error_Code> type_result
        = check_binary_operator(lhs.type, op, rhs.type);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
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
        if ((op == Token_Type::division || op == Token_Type::remainder) && rhs.as_int() == 0) {
            return Evaluation_Error_Code::division_by_zero;
        }
        if ((op == Token_Type::shift_left || op == Token_Type::shift_right)
            && (rhs.as_int() < 0 || rhs.as_int() >= lhs.type.width())) {
            return Evaluation_Error_Code::shift_too_much;
        }
    }

    return Value::unknown_of_type(*type_result);
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error_Code> type_result
        = check_if_expression(lhs.type, condition.type, rhs.type);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    if (condition.is_unknown()) {
        return Value::unknown_of_type(*type_result);
    }
    const auto [result, lossy] = (condition.as_bool() ? lhs : rhs).convert_to(*type_result);
    if (lossy) {
        return Evaluation_Error_Code::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Value> args) noexcept
{
    auto type_result = check_builtin_function(f, args);
    if (!type_result) {
        return Evaluation_Error_Code::type_error;
    }
    if (std::ranges::any_of(args, &Value::is_unknown)) {
        return Value::unknown_of_type(*type_result);
    }
    auto eval_result = unsafe_evaluate_builtin_function_impl(
        f, args | std::views::transform(&Value::concrete_value));
    if (!eval_result) {
        return eval_result.error();
    }
    return Value { *eval_result };
}

} // namespace bit_manipulation::bms