#ifndef BIT_MANIPULATION_ASSERT_HPP
#define BIT_MANIPULATION_ASSERT_HPP

#include <source_location>
#include <string_view>

namespace bit_manipulation {

enum struct Assertion_Error_Type { expression, unreachable };

struct Assertion_Error {
    Assertion_Error_Type type;
    std::string_view message;
    std::source_location location;
};

// Expects an expression.
// If this expression (after contextual conversion to `bool`) is `false`,
// throws an `Assertion_Error` of type `expression`.
#define BIT_MANIPULATION_ASSERT(...)                                                               \
    ((__VA_ARGS__) ? void()                                                                        \
                   : throw ::bit_manipulation::Assertion_Error {                                   \
                       ::bit_manipulation::Assertion_Error_Type::expression, (#__VA_ARGS__),       \
                       ::std::source_location::current() })

/// Expects a string literal.
/// Unconditionally throws `Assertion_Error` of type `unreachable`.
#define BIT_MANIPULATION_ASSERT_UNREACHABLE(...)                                                   \
    (throw ::bit_manipulation::Assertion_Error {                                                   \
        ::bit_manipulation::Assertion_Error_Type::unreachable, ::std::string_view(__VA_ARGS__),    \
        ::std::source_location::current() })

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
