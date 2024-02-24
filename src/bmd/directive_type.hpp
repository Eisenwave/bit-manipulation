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

/// @brief Checks whether the directive type corresponds to a directive that cannot have any
/// block content, such as `\br` or `\hr`.
/// @param type the directive type
/// @return `true` if the corresponding directive must be empty, `false` otherwise.
inline bool directive_type_must_be_empty(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case line_break:
    case horizontal_rule: return true;
    default: return false;
    }
}

/// @brief Returns the formatting style of the directive type.
/// For example, `bold` is formatted as `in_line` whereas `note` is formatted as `block`.
/// @param type the directive type
/// @return The corresponding `Formatting_Style`.
Formatting_Style directive_type_formatting_style(Directive_Type type);

/// @brief Returns the `Directive_Type` corresponding to the identifier in a document.
/// For example, `directive_type_by_id("b")` yields `bold`.
/// @param directive_id the directive identifier
/// @return The corresponding `Directive_Type` if one exists, `std::nullopt` otherwise.
std::optional<Directive_Type> directive_type_by_id(std::string_view directive_id) noexcept;

/// @brief Returns the corresponding HTML tag string for a given `Directive_Type`.
/// @param type the directive type
/// @return The corresponding HTML tag.
std::string_view directive_type_tag(Directive_Type type);

/// @brief Checks whether the corresponding directive is an "HTML passthrough directive".
/// That is, a directive which has no unique rules and simply transforms into an HTML tag.
/// For example, `\b{...}` directly translates into `<b>...</b>`.
inline bool directive_type_is_html_passthrough(Directive_Type type)
{
    return type <= Directive_Type::unordered_list;
}

} // namespace bit_manipulation::bmd

#endif
