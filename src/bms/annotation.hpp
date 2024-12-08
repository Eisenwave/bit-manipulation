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

enum struct Annotation_Parameter_Type : Default_Underlying { boolean, integer, string };

[[nodiscard]] constexpr std::string_view annotation_type_name(Annotation_Type type);

[[nodiscard]] std::optional<Annotation_Type> annotation_type_by_name(std::string_view name);

using Annotation_Argument_Variant = Variant<bool, Big_Int, std::string_view>;

struct Annotation_Argument : Annotation_Argument_Variant {
    using Variant::Variant;
};

static_assert(std::is_trivially_copyable_v<Annotation_Argument>);

struct Annotation {
    Annotation_Type type;
    std::pmr::vector<Annotation_Argument> arguments;
};

} // namespace bit_manipulation::bms

#endif
