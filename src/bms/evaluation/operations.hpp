#ifndef BIT_MANIPULATION_BMS_OPERATIONS_HPP
#define BIT_MANIPULATION_BMS_OPERATIONS_HPP

#include <span>

#include "common/result.hpp"

#include "bms/fwd.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

// Type-only evaluations.

/// @brief Returns the common type between two types, or `std::nullopt` if there is none.
/// @param lhs the left type
/// @param rhs the right ype
/// @return The common type, `incompatible_types`, or `incompatible_widths`.
[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code> get_common_type(const Concrete_Type& lhs,
                                                                         const Concrete_Type& rhs);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_unary_operator(Expression_Type op, const Concrete_Type& value);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_binary_operator(const Concrete_Type& lhs, Expression_Type op, const Concrete_Type& rhs);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_if_expression(const Concrete_Type& lhs,
                    const Concrete_Type& condition,
                    const Concrete_Type& rhs);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Type> args);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args);

[[nodiscard]] Result<Concrete_Type, Analysis_Error_Code>
check_builtin_function(Builtin_Function f, std::span<const Value> args);

// Concrete evaluations.

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_conversion(const Concrete_Value& value, const Concrete_Type& to);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_unary_operator(Expression_Type op, const Concrete_Value& value);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_binary_operator(const Concrete_Value& lhs, Expression_Type op, const Concrete_Value& rhs);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_if_expression(const Concrete_Value& lhs,
                       const Concrete_Value& condition,
                       const Concrete_Value& rhs);

[[nodiscard]] Result<Concrete_Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Concrete_Value> args);

// Evaluations.

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_conversion(const Value& value,
                                                                       const Concrete_Type& to);

[[nodiscard]] Result<Value, Evaluation_Error_Code> evaluate_unary_operator(Expression_Type op,
                                                                           const Value& value);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_binary_operator(const Value& lhs, Expression_Type op, const Value& rhs);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_if_expression(const Value& lhs, const Value& condition, const Value& rhs);

[[nodiscard]] Result<Value, Evaluation_Error_Code>
evaluate_builtin_function(Builtin_Function f, std::span<const Value> args);

} // namespace bit_manipulation::bms

#endif
