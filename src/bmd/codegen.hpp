#ifndef BIT_MANIPULATION_BMD_CODEGEN_HPP
#define BIT_MANIPULATION_BMD_CODEGEN_HPP

#include <string_view>

#include "bms/fwd.hpp"

#include "bmd/code_string.hpp"

namespace bit_manipulation::bmd {

enum struct Code_Language : Default_Underlying {
    bms,
    c,
    cpp,
    rust,
    java,
    kotlin,
    javascript,
    typescript
};

[[nodiscard]] constexpr std::string_view code_language_name(Code_Language lang)
{
    using enum Code_Language;
    switch (lang) {
        BIT_MANIPULATION_ENUM_STRING_CASE(bms);
        BIT_MANIPULATION_ENUM_STRING_CASE(c);
        BIT_MANIPULATION_ENUM_STRING_CASE(cpp);
        BIT_MANIPULATION_ENUM_STRING_CASE(rust);
        BIT_MANIPULATION_ENUM_STRING_CASE(java);
        BIT_MANIPULATION_ENUM_STRING_CASE(kotlin);
        BIT_MANIPULATION_ENUM_STRING_CASE(javascript);
        BIT_MANIPULATION_ENUM_STRING_CASE(typescript);
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid lang");
}

[[nodiscard]] constexpr std::string_view code_language_readable_name(Code_Language lang)
{
    using enum Code_Language;
    switch (lang) {
    case bms: return "BMS";
    case c: return "C";
    case cpp: return "C++";
    case rust: return "Rust";
    case java: return "Java";
    case kotlin: return "Kotlin";
    case javascript: return "JavaScript";
    case typescript: return "TypeScript";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("invalid lang");
}

void generate_code(Code_String& out, const bms::Analyzed_Program& program, Code_Language language);

} // namespace bit_manipulation::bmd

#endif
