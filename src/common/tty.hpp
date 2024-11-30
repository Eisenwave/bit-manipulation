#ifndef BIT_MANIPULATION_TTY_HPP
#define BIT_MANIPULATION_TTY_HPP

#include <cstdio>

namespace bit_manipulation {

// https://pubs.opengroup.org/onlinepubs/009695399/functions/isatty.html
bool is_tty(std::FILE*) noexcept;

} // namespace bit_manipulation

#endif
