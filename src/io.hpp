#ifndef BIT_MANIPULATION_IO_HPP
#define BIT_MANIPULATION_IO_HPP

#include <memory_resource>
#include <string>
#include <string_view>

#include "result.hpp"

namespace bit_manipulation {

enum struct IO_Error_Code {
    cannot_open,
    read_error,
    write_error,
};

Result<std::pmr::string, IO_Error_Code> file_to_string(std::string_view path,
                                                       std::pmr::memory_resource* memory);

} // namespace bit_manipulation

#endif