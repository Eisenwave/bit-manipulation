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

const bool is_stdin_tty = is_tty(stdin);
const bool is_stdout_tty = is_tty(stdout);
const bool is_stderr_tty = is_tty(stderr);

} // namespace bit_manipulation
