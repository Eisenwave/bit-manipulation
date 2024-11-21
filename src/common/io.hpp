#ifndef BIT_MANIPULATION_IO_HPP
#define BIT_MANIPULATION_IO_HPP

#include <memory_resource>
#include <string>
#include <string_view>

#include "common/io_error.hpp"
#include "common/result.hpp"

namespace bit_manipulation {

Result<std::pmr::string, IO_Error_Code> file_to_string(std::string_view path,
                                                       std::pmr::memory_resource* memory);

} // namespace bit_manipulation

#endif
