#ifndef BIT_MANIPULATION_BMD_DIRECTIVE_TYPE_HPP
#define BIT_MANIPULATION_BMD_DIRECTIVE_TYPE_HPP

#include <compare>
#include <optional>
#include <string_view>

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Builtin_Directive_Type : Default_Underlying {
    /// @brief Bold (`<b>`)
    bold,
    /// @brief BMS function info (meta).
    bms_function,
    /// @brief Inline code (`<code>`) with syntax highlighting.
    /// Use `<tt>` for no highlighting.
    code,
    /// @brief C equivalent (meta).
    c_equivalent,
    /// @brief Comment.
    comment,
    /// @brief Code block
    code_block,
    /// @brief Directive Definition.
    definition,
    /// @brief Line break (`<br>`)
    line_break,
    /// @brief Deleted (`<del>`)
    deleted,
    /// @brief Description list (`<dl>`)
    description_list,
    /// @brief Term in a description list (`<dt>`)
    description_term,
    /// @brief Definition in a description list (`<dd>`)
    description_definition,
    /// @brief Emphasized (`<em>`)
    emphasized,
    /// @brief Heading level 1 (`<h1>`)
    heading1,
    /// @brief Heading level 2 (`<h1>`)
    heading2,
    /// @brief Heading level 3 (`<h1>`)
    heading3,
    /// @brief Heading level 4 (`<h1>`)
    heading4,
    /// @brief Heading level 5 (`<h1>`)
    heading5,
    /// @brief Heading level 6 (`<h1>`)
    heading6,
    /// @brief Horizontal rule (`<hr>`)
    horizontal_rule,
    /// @brief Inserted (`<ins>`)
    inserted,
    /// @brief Instruction.
    instruction,
    /// @brief Italic (`<i>`)
    italic,
    /// @brief Item (`<li>`).
    item,
    /// @brief Keyboard key (`<kbd>`)
    keyboard,
    /// @brief Marked (`<mark>`)
    mark,
    /// @brief Block of meta-information.
    meta,
    /// @brief Note.
    note,
    /// @brief Ordered list (`<ol>`)
    ordered_list,
    /// @brief Quoted (`<q>`)
    quoted,
    /// @brief Raw block.
    raw,
    /// @brief Sample output (`<samp>`)
    sample_output,
    /// @brief Strikethrough (`<s>`)
    strikethrough,
    /// @brief Strong (`<strong>`)
    strong,
    /// @brief Subscript (`<sub>`)
    subscript,
    /// @brief Superscript (`<sup>`)
    superscript,
    /// @brief Teletype (HTML4 `<tt>`, but translated into `<code>`)
    teletype,
    /// @brief Title (meta).
    title,
    /// @brief Underlined (`<u>`)
    underlined,
    /// @brief Unordered list (`<ul>`)
    unordered_list
};

struct Directive_Type {
    std::string_view name;
    std::optional<Builtin_Directive_Type> type;
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

[[nodiscard]] constexpr std::strong_ordering operator<=>(Builtin_Directive_Type x,
                                                         Builtin_Directive_Type y) noexcept
{
    return Default_Underlying(x) <=> Default_Underlying(y);
}

/// @brief Checks whether the directive type corresponds to a directive that cannot have
/// any block content, such as `\br` or `\hr`.
/// @param type the directive type
/// @return `true` if the corresponding directive must be empty, `false` otherwise.
[[nodiscard]] inline bool directive_type_must_be_empty(Builtin_Directive_Type type)
{
    using enum Builtin_Directive_Type;
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
[[nodiscard]] Formatting_Style directive_type_formatting_style(Builtin_Directive_Type type);

[[nodiscard]] Directive_Content_Type directive_type_content_type(Builtin_Directive_Type type);

[[nodiscard]] Directive_Environment directive_type_environment(Builtin_Directive_Type type);

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

[[nodiscard]] inline bool directive_type_allowed_in(Builtin_Directive_Type type,
                                                    Directive_Content_Type content)
{
    return directive_content_allows(content, directive_type_environment(type));
}

/// @brief Returns the `Directive_Type` corresponding to the identifier in a document.
/// For example, `directive_type_by_id("b")` yields `bold`.
/// @param directive_id the directive identifier
/// @return The corresponding `Directive_Type` if one exists, `std::nullopt` otherwise.
[[nodiscard]] std::optional<Builtin_Directive_Type>
directive_type_by_id(std::string_view directive_id) noexcept;

/// @brief Returns the corresponding HTML tag string for a given `Directive_Type`.
/// @param type the directive type
/// @return The corresponding HTML tag.
[[nodiscard]] std::string_view directive_type_tag(Builtin_Directive_Type type);

/// @brief Checks whether the corresponding directive is an "HTML passthrough
/// directive". That is, a directive which has no unique rules and simply transforms
/// into an HTML tag. For example, `\b{...}` directly translates into `<b>...</b>`.
[[nodiscard]] bool directive_type_is_html_passthrough(Builtin_Directive_Type type);

} // namespace bit_manipulation::bmd

#endif
