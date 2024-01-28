#ifndef BIT_MANIPULATION_ASSERT_HPP
#define BIT_MANIPULATION_ASSERT_HPP

#include <source_location>

namespace bit_manipulation {

[[noreturn]] void assert_fail(const char*, std::source_location = std::source_location::current());

#define BIT_MANIPULATION_ASSERT(...)                                                               \
    ((__VA_ARGS__) ? void() : ::bit_manipulation::assert_fail(#__VA_ARGS__))

#if __has_cpp_attribute(assume)
#define M3DP_ASSUME(...) [[assume(__VA_ARGS__)]]
#elif defined(__clang__)
#define M3DP_ASSUME(...) __builtin_assume(__VA_ARGS__)
#else
#define M3DP_ASSUME(...)
#endif

#define M3DP_DEBUG_ASSERT(...)                                                                     \
    M3DP_IF_DEBUG(M3DP_ASSERT(__VA_ARGS__))                                                        \
    M3DP_IF_NOT_DEBUG(M3DP_ASSUME(__VA_ARGS__))

} // namespace bit_manipulation

#endif