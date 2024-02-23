#ifndef BIT_MANIPULATION_ANSI_HPP
#define BIT_MANIPULATION_ANSI_HPP

#include <string_view>

namespace bit_manipulation::ansi {

constexpr std::string_view black = "\x1B[30m";
constexpr std::string_view red = "\x1B[31m";
constexpr std::string_view green = "\x1B[32m";
constexpr std::string_view yellow = "\x1B[33m";
constexpr std::string_view blue = "\x1B[34m";
constexpr std::string_view magenta = "\x1B[35m";
constexpr std::string_view cyan = "\x1B[36m";
constexpr std::string_view white = "\x1B[37m";

constexpr std::string_view h_black = "\x1B[0;90m";
constexpr std::string_view h_red = "\x1B[0;91m";
constexpr std::string_view h_green = "\x1B[0;92m";
constexpr std::string_view h_yellow = "\x1B[0;93m";
constexpr std::string_view h_blue = "\x1B[0;94m";
constexpr std::string_view h_magenta = "\x1B[0;95m";
constexpr std::string_view h_cyan = "\x1B[0;96m";
constexpr std::string_view h_white = "\x1B[0;97m";

constexpr std::string_view reset = "\033[0m";

}; // namespace bit_manipulation::ansi

#endif
