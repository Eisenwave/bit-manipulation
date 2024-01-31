#include "bms_operations.hpp"

namespace bit_manipulation::bms {

namespace {

std::optional<Evaluation_Error> convert_to_equal_type(Value& lhs, Value& rhs)
{
    if (lhs.type.type == Type_Type::Int && rhs.type.type == Type_Type::Uint) {
        const auto [converted, lossy] = lhs.to_uint(rhs.type.width);
        if (lossy) {
            return Evaluation_Error::int_to_uint_range_error;
        }
        lhs = converted;
    }
    else if (lhs.type.type == Type_Type::Uint && rhs.type.type == Type_Type::Int) {
        const auto [converted, lossy] = rhs.to_uint(lhs.type.width);
        if (lossy) {
            return Evaluation_Error::int_to_uint_range_error;
        }
        rhs = converted;
    }
    else if (rhs.type != lhs.type) {
        return Evaluation_Error::incompatible_types;
    }

    return std::nullopt;
}

} // namespace

Evaluation_Result evaluate_unary_operator(Token_Type op, Value value) noexcept
{
    if (!is_unary_operator(op)) {
        return Evaluation_Error::invalid_operator;
    }

    switch (value.type.type) {

    case Type_Type::Void: {
        return Evaluation_Error::void_operation;
    }

    case Type_Type::Bool: {
        if (is_arithmetic_operator(op)) {
            return Evaluation_Error::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Evaluation_Error::bool_bitwise;
        }
        if (op == Token_Type::logical_not) {
            return value.and_then([](Big_Int x) { return x ^ 1; });
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Evaluation_Error::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Evaluation_Error::int_logical;
        }
        if (op == Token_Type::plus) {
            return value;
        }
        if (op == Token_Type::minus) {
            return value.and_then([](Big_Int x) { return -x; });
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    case Type_Type::Uint: {
        if (is_logical_operator(op)) {
            return Evaluation_Error::uint_logical;
        }
        if (op == Token_Type::plus) {
            return value;
        }
        if (op == Token_Type::minus) {
            return value.and_then_uint([](Big_Uint x) -> Big_Uint { return -x; });
        }
        if (op == Token_Type::bitwise_not) {
            return value.and_then_uint([](Big_Uint x) -> Big_Uint { return ~x; });
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

Evaluation_Result evaluate_binary_operator(Value lhs, Token_Type op, Value rhs) noexcept
{
    if (!is_binary_operator(op)) {
        return Evaluation_Error::invalid_operator;
    }

    if (std::optional<Evaluation_Error> error = convert_to_equal_type(lhs, rhs)) {
        return *error;
    }

    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    const struct {
        const Value& x;
        const Value& y;

        Value operator()(bool f(Big_Int, Big_Int)) const
        {
            if (x && y) {
                return { Concrete_Type::Bool, Big_Int(f(*x.value, *y.value)) };
            }
            return Value { Concrete_Type::Bool };
        }
        Value operator()(bool f(Big_Uint, Big_Uint)) const
        {
            if (x && y) {
                return Value { Concrete_Type::Bool, f(Big_Uint(*x.value), Big_Uint(*y.value)) };
            }
            return Value { Concrete_Type::Bool };
        }
        Value operator()(Big_Int f(Big_Int, Big_Int)) const
        {
            if (x && y) {
                return Value { x.type, f(*x.value, *y.value) };
            }
            return Value { x.type };
        }
        Value operator()(Big_Uint f(Big_Uint, Big_Uint)) const
        {
            if (x && y) {
                Big_Uint result = f(Big_Uint(*x.value), Big_Uint(*x.value));
                if (x.type.width != std::numeric_limits<Big_Uint>::digits) {
                    result &= (Big_Uint(1) << x.type.width) - 1;
                }
                return Value { x.type, Big_Int(result) };
            }
            return Value { x.type };
        }

    } and_then { lhs, rhs };

    if (lhs.type.type == Type_Type::Int || lhs.type.type == Type_Type::Uint) {
        switch (op) {
        case Token_Type::division:
        case Token_Type::remainder: {
            if (!rhs) {
                return rhs;
            }
            if (*rhs.value == 0) {
                return Evaluation_Error::division_by_zero;
            }
            if (!lhs) {
                return lhs;
            }
            return op == Token_Type::division ? Value { lhs.type, *lhs.value / *rhs.value }
                                              : Value { lhs.type, *lhs.value % *rhs.value };
        }
        // Relational comparisons don't simply check for bit-equality, so they cannot be handled
        // commonly for both.
        case Token_Type::equals: //
            return and_then([](Big_Int x, Big_Int y) { return x == y; });
        case Token_Type::not_equals: //
            return and_then([](Big_Int x, Big_Int y) { return x != y; });
        default: break;
        }
    }

    switch (lhs.type.type) {

    case Type_Type::Void: {
        return Evaluation_Error::void_operation;
    }

    case Type_Type::Bool: {

        if (is_arithmetic_operator(op)) {
            return Evaluation_Error::bool_arithmetic;
        }
        if (is_bitwise_operator(op)) {
            return Evaluation_Error::bool_bitwise;
        }
        if (is_relational_comparison_operator(op)) {
            return Evaluation_Error::bool_relational_comparison;
        }
        if (op == Token_Type::logical_and) {
            return and_then([](Big_Int x, Big_Int y) { return x && y; });
        }
        if (op == Token_Type::logical_or) {
            return and_then([](Big_Int x, Big_Int y) { return x || y; });
        }
        BIT_MANIPULATION_ASSERT(false);
    }

    case Type_Type::Int: {
        if (is_bitwise_operator(op)) {
            return Evaluation_Error::int_bitwise;
        }
        if (is_logical_operator(op)) {
            return Evaluation_Error::int_logical;
        }
        switch (op) {
        case Token_Type::less_than: //
            return and_then([](Big_Int x, Big_Int y) { return x < y; });
        case Token_Type::greater_than: //
            return and_then([](Big_Int x, Big_Int y) { return x > y; });
        case Token_Type::less_or_equal: //
            return and_then([](Big_Int x, Big_Int y) { return x <= y; });
        case Token_Type::greater_or_equal: //
            return and_then([](Big_Int x, Big_Int y) { return x >= y; });
        case Token_Type::plus: //
            return and_then([](Big_Int x, Big_Int y) { return x + y; });
        case Token_Type::minus: //
            return and_then([](Big_Int x, Big_Int y) { return x - y; });
        case Token_Type::multiplication: //
            return and_then([](Big_Int x, Big_Int y) { return x * y; });

        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    case Type_Type::Uint: {
        if (lhs.type.width != rhs.type.width) {
            return Evaluation_Error::incompatible_widths;
        }
        if (is_logical_operator(op)) {
            return Evaluation_Error::uint_logical;
        }
        switch (op) {
        case Token_Type::less_than: //
            return and_then([](Big_Uint x, Big_Uint y) { return x < y; });
        case Token_Type::greater_than: //
            return and_then([](Big_Uint x, Big_Uint y) { return x > y; });
        case Token_Type::less_or_equal: //
            return and_then([](Big_Uint x, Big_Uint y) { return x <= y; });
        case Token_Type::greater_or_equal: //
            return and_then([](Big_Uint x, Big_Uint y) { return x >= y; });
        case Token_Type::plus: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x + y; });
        case Token_Type::minus: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x - y; });
        case Token_Type::multiplication: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x * y; });
        case Token_Type::bitwise_and: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x & y; });
        case Token_Type::bitwise_or: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x | y; });
        case Token_Type::bitwise_xor: //
            return and_then([](Big_Uint x, Big_Uint y) -> Big_Uint { return x ^ y; });
        case Token_Type::shift_left:
        case Token_Type::shift_right: {
            if (!rhs) {
                return rhs;
            }
            if (Big_Uint(*rhs.value) >= Big_Uint(lhs.type.width)) {
                return Evaluation_Error::shift_too_much;
            }
            return lhs.and_then_uint([op, y = Big_Uint(*rhs.value)](Big_Uint x) -> Big_Uint {
                return op == Token_Type::shift_left ? x << y : x >> y;
            });
        }
        default: BIT_MANIPULATION_ASSERT(false);
        }
    }

    default: BIT_MANIPULATION_ASSERT(false);
    }
}

Evaluation_Result evaluate_if_expression(Value lhs, Value condition, Value rhs) noexcept
{
    if (std::optional<Evaluation_Error> error = convert_to_equal_type(lhs, rhs)) {
        return *error;
    }
    BIT_MANIPULATION_ASSERT(lhs.type == rhs.type);

    if (condition) {
        return *condition.value ? lhs : rhs;
    }
    return Value { lhs.type };
}

} // namespace bit_manipulation::bms