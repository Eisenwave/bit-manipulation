#ifndef BIT_MANIPULATION_BMD_CODEGEN_HPP
#define BIT_MANIPULATION_BMD_CODEGEN_HPP

#include <string_view>

#include "common/result.hpp"

#include "bms/fwd.hpp"

#include "bmd/codegen/generator_error.hpp"
#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Brace_Style : Default_Underlying { allman, k_and_r, stroustrup };

[[nodiscard]] constexpr bool is_break_after_function(Brace_Style style)
{
    return style != Brace_Style::k_and_r;
}

[[nodiscard]] constexpr bool is_break_after_if(Brace_Style style)
{
    return style == Brace_Style::allman;
}

enum struct Return_Type_Policy : Default_Underlying {
    /// @brief Prefer to keep the return types as they were written.
    keep,
    /// @brief Specify return types explicitly, even when this is unnecessary and not considered
    /// idiomatic in the target language.
    ///
    /// For example, this always results in Kotlin output containing `: Unit`
    /// and BMS output containing `-> Void` when those are effectively the return types,
    /// despite it being permitted to omit this.
    always
};

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
    /// @brief If `true`, subexpressions are always parenthesized, even if the language doesn't
    /// strictly require this.
    /// By default (`false`), a "readable" subset of necessary parenthesization is used.
    bool always_parenthesize_subexpressions = false;
    /// @brief The return type policy.
    Return_Type_Policy return_types = Return_Type_Policy::keep;

    /// @brief If `true`, prefer C23 features such as spelling `_Bool` as `bool`.
    bool c_23 = false;
    /// @brief If `true`, prefers `_BitInt(N)` over `uintN_t`.
    /// Only available when `c_23` is `true`, and defaults to the `c_23` setting.
    bool c_prefer_bitint = c_23;
};

[[nodiscard]] Result<void, Generator_Error> generate_code(Code_String& out,
                                                          const bms::Analyzed_Program& program,
                                                          Code_Language language,
                                                          const Code_Options& options = {});

[[nodiscard]] Result<void, Generator_Error>
generate_bms_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_rust_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_c_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_cpp_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_java_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_kotlin_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_javascript_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

[[nodiscard]] Result<void, Generator_Error>
generate_typescript_code(Code_String&, const bms::Analyzed_Program&, const Code_Options&);

} // namespace bit_manipulation::bmd

#endif
