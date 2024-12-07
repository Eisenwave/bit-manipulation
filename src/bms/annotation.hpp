#ifndef BIT_MANIPULATION_BMS_ANNOTATION_HPP
#define BIT_MANIPULATION_BMS_ANNOTATION_HPP

#include <memory_resource>
#include <optional>
#include <string_view>
#include <vector>

#include "common/variant.hpp"

#include "bms/debug_info.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @see https://github.com/Eisenwave/bit-manipulation/wiki/Annotations
enum struct Annotation_Type : Default_Underlying {
    immutable,
    inline_,
    loop_variable,
    loop_step,
    unroll,
    c_equivalent,
    java_equivalent,
    instantiate,
    false_if_unknown,
    remove_if_unused
};

[[nodiscard]] constexpr bool annotation_type_applicable_to(Annotation_Type type,
                                                           Construct construct)
{
    using enum Annotation_Type;

    switch (type) {
    case immutable: //
        return construct == Construct::variable;
    case inline_:
        return construct == Construct::variable || construct == Construct::function_call_expression;
    case loop_variable: //
        return construct == Construct::variable;
    case loop_step: //
        return construct == Construct::assignment;
    case unroll: //
        return construct == Construct::while_statement;
    case c_equivalent: //
        return construct == Construct::function;
    case java_equivalent: //
        return construct == Construct::function;
    case instantiate: //
        return construct == Construct::function;
    case false_if_unknown: //
        return construct == Construct::if_statement;
    case remove_if_unused: //
        return construct == Construct::function;
    }
    return false;
}

[[nodiscard]] constexpr std::string_view annotation_type_name(Annotation_Type type);

[[nodiscard]] std::optional<Annotation_Type> annotation_type_by_name(std::string_view name);

using Annotation_Argument_Variant = Variant<Big_Int, std::string_view>;

struct Annotation_Argument : Annotation_Argument_Variant {
    using Variant::Variant;
};

struct Annotation {
    Annotation_Type type;
    std::pmr::vector<Annotation_Argument> arguments;
};

} // namespace bit_manipulation::bms

#endif
