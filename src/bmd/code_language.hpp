#ifndef BIT_MANIPULATION_BMD_CODE_LANGUAGE_HPP
#define BIT_MANIPULATION_BMD_CODE_LANGUAGE_HPP

#include <optional>
#include <string_view>

#include "common/config.hpp"

namespace bit_manipulation::bmd {

enum struct Code_Language : Default_Underlying {
    /// @brief Plaintext.
    plaintext,
    /// @brief Bit Manipulation Docs
    bmd,
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
    typescript,
};

[[nodiscard]] std::string_view code_language_name(Code_Language lang);

[[nodiscard]] std::string_view code_language_readable_name(Code_Language lang);

[[nodiscard]] std::optional<Code_Language> code_language_by_name(std::string_view name);

/// @brief Returns the language corresponding to the given file path (absolute or relative).
/// For example, if `name` is `code.c`, the result is `c`.
///
/// If no language could be identified, `plaintext` is returned as a fallback.
[[nodiscard]] Code_Language code_language_by_file(std::string_view name);

} // namespace bit_manipulation::bmd

#endif
