#include "bmd/codegen/codegen.hpp"
#include "bmd/code_language.hpp"

namespace bit_manipulation::bmd {

Result<void, Generator_Error> generate_rust_code(Code_String&,
                                                 const bms::Analyzed_Program&,
                                                 std::pmr::memory_resource*,
                                                 const Code_Options&)
{
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, Rust codegen not implemented yet");
}

Result<void, Generator_Error> generate_java_code(Code_String&,
                                                 const bms::Analyzed_Program&,
                                                 std::pmr::memory_resource*,
                                                 const Code_Options&)
{
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, Java codegen not implemented yet");
}

Result<void, Generator_Error> generate_kotlin_code(Code_String&,
                                                   const bms::Analyzed_Program&,
                                                   std::pmr::memory_resource*,
                                                   const Code_Options&)
{
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, Kotlin codegen not implemented yet");
}

Result<void, Generator_Error> generate_javascript_code(Code_String&,
                                                       const bms::Analyzed_Program&,
                                                       std::pmr::memory_resource*,
                                                       const Code_Options&)
{
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, JavaScript codegen not implemented yet");
}

Result<void, Generator_Error> generate_typescript_code(Code_String&,
                                                       const bms::Analyzed_Program&,
                                                       std::pmr::memory_resource*,
                                                       const Code_Options&)
{
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Sorry, TypeScript codegen not implemented yet");
}

Result<void, Generator_Error> generate_code(Code_String& out,
                                            const bms::Analyzed_Program& program,
                                            Code_Language language,
                                            std::pmr::memory_resource* memory,
                                            const Code_Options& options)
{
    using enum Code_Language;
    switch (language) {
    case bms: return generate_bms_code(out, program, memory, options);
    case bms_vm: return generate_bms_vm_code(out, program, memory, options);
    case c: return generate_c_code(out, program, memory, options);
    case cpp: return generate_cpp_code(out, program, memory, options);
    case rust: return generate_rust_code(out, program, memory, options);
    case java: return generate_java_code(out, program, memory, options);
    case kotlin: return generate_kotlin_code(out, program, memory, options);
    case javascript: return generate_javascript_code(out, program, memory, options);
    case typescript: return generate_typescript_code(out, program, memory, options);
    default: return Generator_Error { Generator_Error_Code::unsupported_language };
    }
}

} // namespace bit_manipulation::bmd
