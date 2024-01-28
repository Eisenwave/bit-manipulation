#ifndef BIT_MANIPULATION_ANSI_HPP
#define BIT_MANIPULATION_ANSI_HPP

#include <string_view>

namespace bit_manipulation::ansi {

constexpr std::string_view black = "\x1B[30m";
constexpr std::string_view red = "\x1B[31m";
constexpr std::string_view green = "\x1B[32m";
constexpr std::string_view yellow = "\x1B[33m";
constexpr std::string_view reset = "\033[0m";

}; // namespace bit_manipulation::ansi

#endif