#ifndef BIT_MANIPULATION_BMD_GENERATOR_ERROR_HPP
#define BIT_MANIPULATION_BMD_GENERATOR_ERROR_HPP

#include "bms/fwd.hpp"

namespace bit_manipulation::bmd {

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

} // namespace bit_manipulation::bmd

#endif
