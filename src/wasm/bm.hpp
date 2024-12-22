#ifndef BIT_MANIPULATION_WEB_BM_HPP
#define BIT_MANIPULATION_WEB_BM_HPP

#include "common/config.hpp"

#ifdef BIT_MANIPULATION_EMSCRIPTEN
#include <emscripten.h>

extern "C" {

/// @brief Result type for various functions which allocate memory.
struct [[nodiscard]] bm_allocation {
    /// @brief A pointer to the allocated memory.
    void* memory;
    /// @brief The size of the allocation.
    bit_manipulation::Uint32 size;
};

struct [[nodiscard]] bm_text_result {
    /// @brief The result allocation.
    bm_allocation allocation;
    /// @brief `true` if the resulting allocation contains HTML,
    /// `false` if it contains plaintext.
    bool is_html;
};

// IntelliSense doesn't seem capable of figuring out we're building for a 32-bit target from
// compile_commands.json (which indicate this via use of em++).
// However, bm_allocation should absolutely be 8 bytes large on a 32-bit target.
#ifndef __INTELLISENSE__
static_assert(sizeof(bm_allocation) == 8);
#endif

/// @brief Allocates memory in WASM.
/// Allocated memory must be freed using `bm_free`.
/// This form of allocation is only intended to be used by JavaScript code
/// which needs to pass dynamic data to the bit manipulation library.
/// @param n the amount of bytes to allocate
[[nodiscard]] void* bm_foreign_alloc(bit_manipulation::Uint32 n);

/// @brief Frees memory which has been previously allocated with `bm_alloc`.
/// @param p the pointer to free;
/// must be the result of a prior call to `bm_foreign_alloc`, and must not be freed already
/// @param n the amount of bytes to free;
/// must be the argument previously passed to `bm_foreign_alloc`
void bm_foreign_free(void* p, bit_manipulation::Uint32 n);

int bm_plus(int, int);

extern bm_text_result bm_length_as_string_result;

void bm_length_as_string(const char* str);

extern bm_text_result bm_translate_code_result;

/// @brief Translates BMS code in the string to the specified target language.
/// @param source the BMS source code
/// @param source_length the length of the source code
/// @param lang the language to convert to, matching the values in `bmd::Code_Language`
void bm_translate_code(const char* source,
                       bit_manipulation::Uint32 source_length,
                       bit_manipulation::Uint8 lang);

extern bm_allocation bm_syntax_highlight_result;

/// @brief Takes the input code and converts it to HTML, stored in `bm_syntax_highlight_result`.
/// This function makes use of error recovery techniques,
/// so even if some of the code is incorrect BMS, some syntax highlighting is still available.
/// @param source the BMS source code
/// @param source_length the length of the source code
void bm_syntax_highlight(const char* source, bit_manipulation::Uint32 source_length);

//
}

#endif

#endif
