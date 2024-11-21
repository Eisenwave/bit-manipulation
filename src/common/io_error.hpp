#ifndef BIT_MANIPULATION_IO_ERROR_HPP
#define BIT_MANIPULATION_IO_ERROR_HPP

namespace bit_manipulation {

enum struct IO_Error_Code {
    cannot_open,
    read_error,
    write_error,
};

} // namespace bit_manipulation

#endif
