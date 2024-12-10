#include <charconv>
#include <cstring>
#include <memory_resource>
#include <string>

#include "common/assert.hpp"
#include "common/config.hpp"

#include "wasm/bm.hpp"

namespace bit_manipulation {
namespace {

bm_allocation copy_to_heap(const void* data, Uint32 n)
{
    void* memory = bm_foreign_alloc(n);
    if (memory != nullptr) {
        std::memcpy(memory, data, n);
    }
    return { memory, n };
}

[[maybe_unused]] bm_allocation copy_to_heap(const std::string& str)
{
    return copy_to_heap(str.data(), Uint32(str.size() + 1));
}

} // namespace
} // namespace bit_manipulation

using namespace bit_manipulation;

extern "C" {

static std::pmr::unsynchronized_pool_resource foreign_allocations_resource;

void* bm_foreign_alloc(Uint32 n)
{
    return foreign_allocations_resource.allocate(n);
}

void bm_foreign_free(void* p, Uint32 n)
{
    return foreign_allocations_resource.deallocate(p, n);
}

int bm_plus(int x, int y)
{
    return x + y;
}

void bm_length_as_string(const char* str)
{
    char buffer[32];
    auto result = std::to_chars(buffer, std::end(buffer), std::strlen(str));
    BIT_MANIPULATION_ASSERT(result.ec == std::error_code {});

    bm_length_as_string_result = copy_to_heap(buffer, Uint32(result.ptr - buffer));
}

//
}
