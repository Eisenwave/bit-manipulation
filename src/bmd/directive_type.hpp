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

/// @brief The type of content which is allowed in a given directive.
/// For example, `\b` (i.e. `bold`) cannot be used to make multiple paragraphs bold.
/// It is restricted to `span`, i.e. what would be considered a single paragraph.
enum struct Directive_Content_Type : Default_Underlying {
    /// @brief Nothing allowed inside.
    /// Examples includes `\br` and `\hr`.
    nothing,
    /// @brief Text only, no directives and no paragraphs.
    /// For example, `\title` does not support any directives.
    text_span,
    /// @brief A single paragraph, possibly containing directives.
    span,
    /// @brief Multiple paragraphs, possibly containing directives.
    block,
    /// @brief Only directives with `Directive_Environment::meta`, and no text.
    meta,
    /// @brief Only directives with `Directive_Environment::list`, and no text.
    list,
    /// @brief Raw content whose end is determined through braces, but which is not traditionally
    /// parsed as BMD.
    raw,
};

/// @brief The environment that a directive is considered to be in, or allowed to be in.
enum struct Directive_Environment : Default_Underlying {
    /// @brief The directive is considered to be inside the surrounding paragraph.
    paragraph,
    /// @brief The directive is considered to be its own paragraph.
    /// For example, if a `heading1` is found anywhere within a paragraph,
    /// the paragraph is broken up so that `heading1` exists in isolation.
    content,
    /// @brief The directive can only appear within a `\meta` directive.
    meta,
    /// @brief The directive can only appear within lists (`\ul` et al.)
    list,
};

[[nodiscard]] constexpr std::strong_ordering operator<=>(Directive_Type x,
                                                         Directive_Type y) noexcept
{
    return static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
}

/// @brief Checks whether the directive type corresponds to a directive that cannot have
/// any block content, such as `\br` or `\hr`.
/// @param type the directive type
/// @return `true` if the corresponding directive must be empty, `false` otherwise.
[[nodiscard]] inline bool directive_type_must_be_empty(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case line_break:
    case horizontal_rule: return true;
    default: return false;
    }
}

/// @brief Returns the formatting style of the directive type.
/// For example, `bold` is formatted as `in_line` whereas `note` is formatted as
/// `block`.
/// @param type the directive type
/// @return The corresponding `Formatting_Style`.
[[nodiscard]] Formatting_Style directive_type_formatting_style(Directive_Type type);

[[nodiscard]] Directive_Content_Type directive_type_content_type(Directive_Type type);

[[nodiscard]] Directive_Environment directive_type_environment(Directive_Type type);

[[nodiscard]] inline bool directive_content_allows_directives(Directive_Content_Type type)
{
    return type != Directive_Content_Type::nothing //
        && type != Directive_Content_Type::text_span;
}

[[nodiscard]] inline bool directive_content_allows_only_directives(Directive_Content_Type type)
{
    return type == Directive_Content_Type::meta || type == Directive_Content_Type::list;
}

/// @brief Checks whether a block inside a given directive is allowed to contain a
/// directive whose environment is the given environment.
/// @param content the content type of the surrounding directive
/// @param environment the environment that the inner directive is allowed in
/// @return `true` if the directive with the given `environment` is allowed in a
/// directive with the given `content`, `false` otherwise.
[[nodiscard]] bool directive_content_allows(Directive_Content_Type content,
                                            Directive_Environment environment);

[[nodiscard]] inline bool directive_type_allowed_in(Directive_Type type,
                                                    Directive_Content_Type content)
{
    return directive_content_allows(content, directive_type_environment(type));
}

/// @brief Returns the `Directive_Type` corresponding to the identifier in a document.
/// For example, `directive_type_by_id("b")` yields `bold`.
/// @param directive_id the directive identifier
/// @return The corresponding `Directive_Type` if one exists, `std::nullopt` otherwise.
[[nodiscard]] std::optional<Directive_Type>
directive_type_by_id(std::string_view directive_id) noexcept;

/// @brief Returns the corresponding HTML tag string for a given `Directive_Type`.
/// @param type the directive type
/// @return The corresponding HTML tag.
[[nodiscard]] std::string_view directive_type_tag(Directive_Type type);

/// @brief Checks whether the corresponding directive is an "HTML passthrough
/// directive". That is, a directive which has no unique rules and simply transforms
/// into an HTML tag. For example, `\b{...}` directly translates into `<b>...</b>`.
[[nodiscard]] inline bool directive_type_is_html_passthrough(Directive_Type type)
{
    return type <= Directive_Type::unordered_list;
}

} // namespace bit_manipulation::bmd

#endif
