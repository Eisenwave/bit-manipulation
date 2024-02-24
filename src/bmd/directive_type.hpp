#ifndef BIT_MANIPULATION_BMD_DIRECTIVE_TYPE_HPP
#define BIT_MANIPULATION_BMD_DIRECTIVE_TYPE_HPP

#include <compare>
#include <optional>
#include <string_view>

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Directive_Type : Default_Underlying {
    // HTML PASSTHROUGH TAGS
    bold,
    line_break,
    deleted,
    heading1,
    heading2,
    heading3,
    heading4,
    heading5,
    heading6,
    horizontal_rule,
    italic,
    keyboard,
    ordered_list,
    quoted,
    strikethrough,
    subscript,
    superscript,
    teletype,
    underlined,
    unordered_list,
    // BMD EXTRA TAGS
    code,
    code_block,
    instruction,
    item,
    note,
    meta,
    title,
    bms_function,
    c_equivalent,
};

constexpr std::strong_ordering operator<=>(Directive_Type x, Directive_Type y) noexcept
{
    return static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
}

inline bool directive_type_is_html_passthrough(Directive_Type type)
{
    return type <= Directive_Type::unordered_list;
}

inline bool directive_type_must_be_empty(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case line_break:
    case horizontal_rule: return true;
    default: return false;
    }
}

Formatting_Style directive_type_formatting_style(Directive_Type type);

std::optional<Directive_Type> directive_type_by_id(std::string_view directive_id) noexcept;

std::string_view directive_type_tag(Directive_Type type);

} // namespace bit_manipulation::bmd

#endif
