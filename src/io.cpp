#include "io.hpp"
#include "config.hpp"

#include <fstream>

namespace bit_manipulation {

Result<std::string, IO_Error_Code> file_to_string(std::string_view path)
{
    constexpr auto read_size = std::size_t(4096);
    auto stream = std::ifstream(path.data());
    stream.exceptions(std::ios_base::badbit);

    if (!stream) {
        return IO_Error_Code::cannot_open;
    }

    auto out = std::string();
    auto buf = std::string(read_size, '\0');
    while (stream.read(&buf[0], read_size)) {
        out.append(buf, 0, static_cast<Size>(stream.gcount()));
    }
    out.append(buf, 0, static_cast<Size>(stream.gcount()));
    return out;
}

} // namespace bit_manipulation