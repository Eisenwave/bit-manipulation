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

bm_allocation bm_length_as_string_result;

void bm_length_as_string(const char* str);

//
}

#endif

#endif
