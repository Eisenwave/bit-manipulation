#include <cstdio>
#include <cstring>

#include "common/assert.hpp"
#include "common/config.hpp"
#include "common/io.hpp"

namespace bit_manipulation {

Result<std::pmr::vector<char>, IO_Error_Code> file_to_bytes(std::string_view path,
                                                            std::pmr::memory_resource* memory)
{
    constexpr Size block_size = 4096;
    char buffer[block_size] {};

    BIT_MANIPULATION_ASSERT(path.size() < block_size);
    std::memcpy(buffer, path.data(), path.size());

    auto stream = std::fopen(buffer, "rb");
    if (!stream) {
        return IO_Error_Code::cannot_open;
    }

    std::pmr::vector<char> out(memory);
    Size read_size;
    do {
        read_size = std::fread(buffer, 1, block_size, stream);
        if (std::ferror(stream)) {
            return IO_Error_Code::read_error;
        }
        out.insert(out.end(), buffer, buffer + read_size);
    } while (read_size == block_size);

    return out;
}

} // namespace bit_manipulation
