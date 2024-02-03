#include "bms/operations.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

namespace {

[[nodiscard]] bool convert_to_equal_type(Concrete_Type& lhs, Concrete_Type& rhs)
{
    if (lhs.type == rhs.type) {
        return true;
    }
    if (lhs.type == Type_Type::Int && rhs.type == Type_Type::Uint) {
        lhs = rhs;
        return true;
    }
    else if (lhs.type == Type_Type::Uint && rhs.type == Type_Type::Int) {
        rhs = lhs;
        return true;
    }

    return false;
}

template <typename T>
[[nodiscard]] Result<void, Evaluation_Error> convert_to_equal_type_impl(T& lhs, T& rhs)
{
    if (!convert_to_equal_type(lhs.type, rhs.type)) {
        return Evaluation_Error::type_error;
    }

    Concrete_Type lhs_type = lhs.type;
    Concrete_Type rhs_type = rhs.type;

    if (auto [converted, lossy] = lhs.convert_to(lhs_type); !lossy) {
        lhs = converted;
    }
    else {
        return Evaluation_Error::int_to_uint_range_error;
    }

    if (auto [converted, lossy] = lhs.convert_to(rhs_type); !lossy) {
        lhs = converted;
    }
    else {
        return Evaluation_Error::int_to_uint_range_error;
    }
    return {};
}

[[nodiscard]] Result<void, Evaluation_Error> convert_to_equal_type(Concrete_Value& lhs,
                                                                   Concrete_Value& rhs)
{
    return convert_to_equal_type_impl(lhs, rhs);
}

[[nodiscard]] Result<void, Evaluation_Error> convert_to_equal_type(Value& lhs, Value& rhs)
{
    return convert_to_equal_type_impl(lhs, rhs);
}

[[nodiscard]] Result<Value, Evaluation_Error>
result_from_concrete(Result<Concrete_Value, Evaluation_Error> result)
{
    if (result) {
        return Value { result.value() };
    }
    else {
        return result.error();
    }
}

} // namespace

// TYPE ============================================================================================

[[nodiscard]] Result<Concrete_Type, Type_Error> check_unary_operator(Token_Type op,
                                                                     Concrete_Type value) noexcept
{
    if (!is_unary_operator(op)) {
        return Type_Error::invalid_operator;
    }

    switch (value.type) {

    case Type_Type::Void: {
        return Type_Error::void_operation;
    }

    case Type_Type::Bool: {
        if (is_arithmetic_operator(op)) {
            return Type_Error::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Type_Error::bool_bitwise;
        }
        return value;
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Type_Error::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Type_Error::int_logical;
        }
        return value;
    }

    case Type_Type::Uint: {
        if (is_logical_operator(op)) {
            return Type_Error::uint_logical;
        }
        return value;
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

[[nodiscard]] Result<Concrete_Type, Type_Error>
check_binary_operator(Concrete_Type lhs, Token_Type op, Concrete_Type rhs) noexcept
{
    if (!is_binary_operator(op)) {
        return Type_Error::invalid_operator;
    }

    if (!convert_to_equal_type(lhs, rhs)) {
        return Type_Error::incompatible_types;
    }

    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    switch (lhs.type) {

    case Type_Type::Void: {
        return Type_Error::void_operation;
    }

    case Type_Type::Bool: {
        if (is_arithmetic_operator(op)) {
            return Type_Error::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Type_Error::bool_bitwise;
        }
        if (is_relational_comparison_operator(op)) {
            return Type_Error::bool_relational_comparison;
        }
        return Concrete_Type::Bool;
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Type_Error::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Type_Error::int_logical;
        }
        return is_comparison_operator(op) ? Concrete_Type::Bool : lhs;
    }

    case Type_Type::Uint: {
        if (lhs.width != rhs.width) {
            return Type_Error::incompatible_widths;
        }
        if (is_logical_operator(op)) {
            return Type_Error::uint_logical;
        }
        return is_comparison_operator(op) ? Concrete_Type::Bool : lhs;
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

[[nodiscard]] Result<Concrete_Type, Type_Error>
check_if_expression(Concrete_Type lhs, Concrete_Type condition, Concrete_Type rhs) noexcept
{
    if (condition != Concrete_Type::Bool) {
        return Type_Error::condition_not_bool;
    }
    if (!convert_to_equal_type(lhs, rhs)) {
        return Type_Error::incompatible_types;
    }
    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    return lhs;
}

// CONCRETE VALUE ==================================================================================

[[nodiscard]] Result<Concrete_Value, Evaluation_Error>
evaluate_conversion(Concrete_Value value, Concrete_Type to) noexcept
{
    if (!value.type.is_convertible_to(to)) {
        return Evaluation_Error::type_error;
    }

    auto [result, lossy] = value.convert_to(to);
    if (lossy) {
        return Evaluation_Error::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error>
evaluate_unary_operator(Token_Type op, Concrete_Value value) noexcept
{
    if (Result<Concrete_Type, Type_Error> r = check_unary_operator(op, value.type); !r) {
        return Evaluation_Error::type_error;
    }

    switch (value.type.type) {

    case Type_Type::Bool: {
        if (op == Token_Type::logical_not) {
            return Concrete_Value { Concrete_Type::Bool, value.int_value ^ 1 };
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    case Type_Type::Int: {
        if (op == Token_Type::plus) {
            return value;
        }
        if (op == Token_Type::minus) {
            return Concrete_Value { Concrete_Type::Int, -value.int_value };
        }
        BIT_MANIPULATION_ASSERT(false);
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
        BIT_MANIPULATION_ASSERT(false);
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error>
evaluate_binary_operator(Concrete_Value lhs, Token_Type op, Concrete_Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error> target_type = check_binary_operator(lhs.type, op, rhs.type);
    if (!target_type) {
        return Evaluation_Error::type_error;
    }

    if (Result<void, Evaluation_Error> r = convert_to_equal_type(lhs, rhs); !r) {
        return r.error();
    }

    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    const auto compare_uint = [&lhs, &rhs](bool f(Big_Uint, Big_Uint)) -> Concrete_Value {
        const Big_Int result = Big_Int(f(Big_Uint(lhs.int_value), Big_Uint(rhs.int_value)));
        return Concrete_Value { Concrete_Type::Bool, result };
    };

    const auto transform_uint = [&lhs, &rhs](Big_Uint f(Big_Uint, Big_Uint)) -> Concrete_Value {
        Big_Uint result = f(Big_Uint(lhs.int_value), Big_Uint(rhs.int_value));
        if (lhs.type.width != std::numeric_limits<Big_Uint>::digits) {
            result &= (Big_Uint(1) << rhs.type.width) - 1;
        }
        return Concrete_Value { lhs.type, Big_Int(result) };
    };

    if (lhs.type.type == Type_Type::Int || lhs.type.type == Type_Type::Uint) {
        switch (op) {
        case Token_Type::division:
        case Token_Type::remainder: {
            if (rhs.int_value == 0) {
                return Evaluation_Error::division_by_zero;
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

    switch (lhs.type.type) {
    case Type_Type::Bool: {
        if (op == Token_Type::logical_and) {
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value && rhs.int_value) };
        }
        if (op == Token_Type::logical_or) {
            return Concrete_Value { Concrete_Type::Bool, Big_Int(lhs.int_value || rhs.int_value) };
        }
        BIT_MANIPULATION_ASSERT(false);
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
        default: BIT_MANIPULATION_ASSERT(false);
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
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

[[nodiscard]] Result<Concrete_Value, Evaluation_Error>
evaluate_if_expression(Concrete_Value lhs, Concrete_Value condition, Concrete_Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error> type_result
        = check_if_expression(lhs.type, condition.type, rhs.type);
    if (!type_result) {
        return Evaluation_Error::type_error;
    }
    const auto [result, lossy] = (condition.int_value ? lhs : rhs).convert_to(*type_result);
    if (lossy) {
        return Evaluation_Error::int_to_uint_range_error;
    }
    return result;
}

// VALUE ===========================================================================================

[[nodiscard]] Result<Value, Evaluation_Error> evaluate_conversion(Value value,
                                                                  Concrete_Type to) noexcept
{
    if (!value.type.is_convertible_to(to)) {
        return Evaluation_Error::type_error;
    }

    auto [result, lossy] = value.convert_to(to);
    if (lossy) {
        return Evaluation_Error::int_to_uint_range_error;
    }
    return result;
}

[[nodiscard]] Result<Value, Evaluation_Error> evaluate_unary_operator(Token_Type op,
                                                                      Value value) noexcept
{
    if (Result<Concrete_Type, Type_Error> r = check_unary_operator(op, value.type); !r) {
        return Evaluation_Error::type_error;
    }
    if (!value.int_value) {
        return value;
    }
    return result_from_concrete(evaluate_unary_operator(op, value.concrete_value()));
}

[[nodiscard]] Result<Value, Evaluation_Error>
evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept
{
    const Result<Concrete_Type, Type_Error> type_result
        = check_binary_operator(lhs.type, op, rhs.type);
    if (!type_result) {
        return Evaluation_Error::type_error;
    }
    if (Result<void, Evaluation_Error> r = convert_to_equal_type(lhs, rhs); !r) {
        return r.error();
    }
    if (lhs && rhs) {
        return result_from_concrete(
            evaluate_binary_operator(lhs.concrete_value(), op, rhs.concrete_value()));
    }
    // Even if we don't know the values of both operands, there are certain operations which are
    // illegal no matter what the other operand is, such as division by zero.
    if (rhs) {
        if ((op == Token_Type::division || op == Token_Type::remainder) && *rhs.int_value == 0) {
            return Evaluation_Error::division_by_zero;
        }
        if ((op == Token_Type::shift_left || op == Token_Type::shift_right)
            && (*rhs.int_value < 0 || *rhs.int_value >= lhs.type.width)) {
            return Evaluation_Error::shift_too_much;
        }
    }

    return Value { *type_result };
}

[[nodiscard]] Result<Value, Evaluation_Error>
evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept
{
    Result<Concrete_Type, Type_Error> type_result
        = check_if_expression(lhs.type, condition.type, rhs.type);
    if (!type_result) {
        return Evaluation_Error::type_error;
    }
    if (!condition.int_value) {
        return Value { *type_result };
    }
    const auto [result, lossy] = (*condition.int_value ? lhs : rhs).convert_to(*type_result);
    if (lossy) {
        return Evaluation_Error::int_to_uint_range_error;
    }
    return result;
}

} // namespace bit_manipulation::bms