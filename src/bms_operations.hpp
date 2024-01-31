#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include "bms_tokens.hpp"
#include "bms_value.hpp"

namespace bit_manipulation::bms {

enum struct Evaluation_Error {
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
    /// @brief Arithmetic with bool was attempted, such as +true.
    int_bitwise,
    /// @brief Logical operators were applied to Int.
    int_logical,
    /// @brief Logical operators were applied to Uint.
    uint_logical,
    /// @brief A binary or n-ary operation with incompatible types was attempted.
    incompatible_types,
    /// @brief A binary or n-ary operation with UInts of different widths was attempted.
    incompatible_widths,
    /// @brief When converting Int to Uint(N), the Int couldn't be represented.
    int_to_uint_range_error,
    /// @brief Division by zero.
    division_by_zero,
    /// @brief Shift by operand size or more.
    shift_too_much,
};

struct Evaluation_Result {
private:
    std::variant<Value, Evaluation_Error> v;

public:
    [[nodiscard]] constexpr Evaluation_Result(Value value) noexcept
        : v(value)
    {
    }

    [[nodiscard]] constexpr Evaluation_Result(Evaluation_Error error) noexcept
        : v(error)
    {
    }

    [[nodiscard]] constexpr bool has_value() const
    {
        return v.index() == 0;
    }

    [[nodiscard]] constexpr Value get_value() const
    {
        return std::get<Value>(v);
    }

    [[nodiscard]] constexpr Evaluation_Error get_error() const
    {
        return std::get<Evaluation_Error>(v);
    }

    [[nodiscard]] constexpr explicit operator bool() const noexcept
    {
        return v.index() == 0;
    }

    [[nodiscard]] Value operator*() const
    {
        return std::get<Value>(v);
    }
};

enum struct Builtin_Operation {
    equals,
    not_equals,
    plus,
    minus,
    multiplication,
    division,
    remainder,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,
    shift_left,
    shift_right,
    bitwise_and,
    bitwise_or,
    bitwise_not,
    bitwise_xor,
    bitwise_not,
    logical_and,
    logical_or,
    logical_not,
};

inline Evaluation_Result evaluate_unary_operator(Token_Type op, Value value)
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

inline Evaluation_Result evaluate_binary_operator(Value lhs, Token_Type op, Value rhs)
{
    if (!is_binary_operator(op)) {
        return Evaluation_Error::invalid_operator;
    }

    if (lhs.type.type == Type_Type::Int && rhs.type.type == Type_Type::Uint) {
        const auto [converted, lossy] = lhs.to_uint(rhs.type.width);
        if (lossy) {
            return Evaluation_Error::int_to_uint_range_error;
        }
        return evaluate_binary_operator(converted, op, rhs);
    }

    if (lhs.type.type == Type_Type::Uint && rhs.type.type == Type_Type::Int) {
        const auto [converted, lossy] = rhs.to_uint(lhs.type.width);
        if (lossy) {
            return Evaluation_Error::int_to_uint_range_error;
        }
        return evaluate_binary_operator(lhs, op, converted);
    }

    // From this point onwards, the types of the operands should be identical.
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
        if (rhs.type.type != lhs.type.type) {
            return Evaluation_Error::incompatible_types;
        }
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

} // namespace bit_manipulation::bms

#endif