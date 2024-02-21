#include "common/config.hpp"

#include "io.hpp"

#include <fstream>

namespace bit_manipulation {

Result<std::pmr::string, IO_Error_Code> file_to_string(std::string_view path,
                                                       std::pmr::memory_resource* memory)
{
    constexpr auto read_size = std::size_t(4096);
    auto stream = std::ifstream(path.data());
    stream.exceptions(std::ios_base::badbit);

    if (!stream) {
        return IO_Error_Code::cannot_open;
    }

    std::pmr::string out(memory);
    // TODO: avoid use of std::pmr::string for buffer
    std::pmr::string buf(read_size, '\0', memory);
    while (stream.read(&buf[0], read_size)) {
        out.append(buf, 0, static_cast<Size>(stream.gcount()));
    }
    out.append(buf, 0, static_cast<Size>(stream.gcount()));
    return out;
}

} // namespace bit_manipulation