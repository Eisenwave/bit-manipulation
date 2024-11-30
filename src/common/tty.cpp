#include "tty.hpp"

#ifdef __unix__
#include <unistd.h>
#endif

namespace bit_manipulation {

bool is_tty(std::FILE* file) noexcept
{
#ifdef __unix__
    return isatty(fileno(file));
#else
    return false;
#endif
}

} // namespace bit_manipulation
