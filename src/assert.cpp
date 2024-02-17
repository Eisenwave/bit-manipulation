#include <source_location>
#include <stdexcept>
#include <string>

#include "ansi.hpp"

namespace bit_manipulation {
namespace {

} // namespace

[[noreturn]] void assert_fail(const char* expression, std::source_location loc)
{
    std::string message;
    message.reserve(256);
    message += loc.file_name();
    message += ':';
    message += std::to_string(loc.line());
    message += ": ";
    message += ansi::red;
    message += expression;
    message += ansi::reset;

    throw std::runtime_error(std::move(message));
}

} // namespace bit_manipulation