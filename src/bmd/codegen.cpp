#include "bms/analyze.hpp"

#include "bmd/code_string.hpp"
#include "bmd/codegen.hpp"

namespace bit_manipulation::bmd {

static_assert(std::ranges::random_access_range<Code_String>);

void generate_code(Code_String& out, const bms::Analyzed_Program& program, Code_Language language)
{
    //
}

} // namespace bit_manipulation::bmd
