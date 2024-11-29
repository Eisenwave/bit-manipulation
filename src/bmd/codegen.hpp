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

[[nodiscard]] std::string_view code_language_name(Code_Language lang);

[[nodiscard]] std::string_view code_language_readable_name(Code_Language lang);

void generate_code(Code_String& out, const bms::Analyzed_Program& program, Code_Language language);

} // namespace bit_manipulation::bmd

#endif
