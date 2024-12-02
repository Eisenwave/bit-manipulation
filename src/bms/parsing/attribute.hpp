#ifndef BIT_MANIPULATION_BMS_ATTRIBUTE_HPP
#define BIT_MANIPULATION_BMS_ATTRIBUTE_HPP

#include <memory_resource>
#include <optional>
#include <string_view>
#include <vector>

#include "common/variant.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Attribute_Type : Default_Underlying {
    /// @brief Applied to a `let` declaration.
    /// Requires that the variable is never modified.
    ///
    /// Example: `@immutable let x_sqr = x * x;`
    immutable,
    /// @brief Applied to a `let` declaration.
    /// Requires that the declaration immediately precedes a `while` loop,
    /// and that the variable is not used after the loop.
    /// During code generation, for languages with a three-statement for loop,
    /// this declaration will be lifted into the loop.
    ///
    /// Example: `@loop_variable let i = 0; while ...`.
    loop_variable,
    /// @brief Applied to an assignment statement.
    /// Requires that the statement is the last statement in a `while` loop.
    /// During code generation, for languages with a three-statement for loop,
    /// this increment will be lifted into the increment, and a `for` loop is emitted instead.
    ///
    /// Example: `@loop_increment i = i + 1;`
    loop_increment,
    /// @brief Applied to a generic function.
    /// Takes a list of integer arguments indicating the widths with which the
    /// function is instantiated.
    ///
    /// Example: `@instantiate(8, 16, 32, 64) function f(x: Uint(N)) ...`.
    instantiate,
};

[[nodiscard]] constexpr std::string_view attribute_type_name(Attribute_Type type);

[[nodiscard]] std::optional<Attribute_Type> attribute_type_by_name(std::string_view name);

using Attribute_Argument_Variant = Variant<std::string_view>;

struct Attribute_Argument : Attribute_Argument_Variant {
    using Variant::Variant;
};

struct Attribute {
    Attribute_Type type;
    std::pmr::vector<Attribute_Argument> arguments;
};

} // namespace bit_manipulation::bms

#endif
