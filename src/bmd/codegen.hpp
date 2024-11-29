#ifndef BIT_MANIPULATION_BMD_CODEGEN_HPP
#define BIT_MANIPULATION_BMD_CODEGEN_HPP

#include <string_view>

#include "bms/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Code_Language : Default_Underlying {
    /// @brief Bit Manipulation Script
    bms,
    /// @brief C
    c,
    /// @brief C++
    cpp,
    /// @brief Rust
    rust,
    /// @brief Java
    java,
    /// @brief Kotlin
    kotlin,
    /// @brief JavaScript
    javascript,
    /// @brief TypeScript
    typescript
};

enum struct Brace_Style { allman, k_and_r, stroustrup };

constexpr bool is_break_after_function(Brace_Style style)
{
    return style != Brace_Style::k_and_r;
}

constexpr bool is_break_after_if(Brace_Style style)
{
    return style == Brace_Style::allman;
}

struct Code_Options {
    /// @brief The character used for indentation.
    char indent_char = ' ';
    /// @brief The repetitions of `indent_char` per indentation level.
    Size indent_size = 4;
    /// @brief The brace style. Overriden by following flags.
    Brace_Style brace_style = Brace_Style::k_and_r;
    /// @brief If `true`, breaks the line after the parameter list.
    bool break_after_function = is_break_after_function(brace_style);
    /// @brief If `true`, breaks the line after the condition.
    bool break_after_if = is_break_after_if(brace_style);
    /// @brief If `true`, prefer C23 features such as spelling `_Bool` as `bool`.
    bool c23 = false;
};

[[nodiscard]] std::string_view code_language_name(Code_Language lang);

[[nodiscard]] std::string_view code_language_readable_name(Code_Language lang);

enum struct Generator_Error_Code : Default_Underlying {
    /// @brief The code construct could not be translated or is irrelevant to the result language.
    /// However, this is not a critical issue and the construct can simply be ignored.
    /// For example, this can happen with `static_assert` when translated into languages that have
    /// no such assertions.
    empty,
    /// @brief An integer with an unsupported width was requested.
    /// For example, this would happen when converting `Uint(3)` to a language that has only the
    /// traditional 8-bit, 16-bit, etc. integers.
    unsupported_integer_width,
    /// @brief The code construct could not be translated, and this makes the resulting code
    /// nonsensical or ill-formed.
    error
};

struct Generator_Error {
    Generator_Error_Code code;
    const bms::ast::Some_Node* fail;
};

bool generate_code(Code_String& out,
                   const bms::Analyzed_Program& program,
                   Code_Language language,
                   const Code_Options& options = {});

} // namespace bit_manipulation::bmd

#endif
