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

#ifdef __EXCEPTIONS
#define BIT_MANIPULATION_RAISE_ASSERTION_ERROR(...) (throw __VA_ARGS__)
#else
#define BIT_MANIPULATION_RAISE_ASSERTION_ERROR(...) ::std::exit(3)
#endif

// Expects an expression.
// If this expression (after contextual conversion to `bool`) is `false`,
// throws an `Assertion_Error` of type `expression`.
#define BIT_MANIPULATION_ASSERT(...)                                                               \
    ((__VA_ARGS__) ? void()                                                                        \
                   : BIT_MANIPULATION_RAISE_ASSERTION_ERROR(::bit_manipulation::Assertion_Error {  \
                         ::bit_manipulation::Assertion_Error_Type::expression, (#__VA_ARGS__),     \
                         ::std::source_location::current() }))

/// Expects a string literal.
/// Unconditionally throws `Assertion_Error` of type `unreachable`.
#define BIT_MANIPULATION_ASSERT_UNREACHABLE(...)                                                   \
    BIT_MANIPULATION_RAISE_ASSERTION_ERROR(::bit_manipulation::Assertion_Error {                   \
        ::bit_manipulation::Assertion_Error_Type::unreachable, ::std::string_view(__VA_ARGS__),    \
        ::std::source_location::current() })

#if __cplusplus >= 202302L
#define BIT_MANIPULATION_CPP23 1
#endif

#if defined(BIT_MANIPULATION_CPP23) && __has_cpp_attribute(assume)
#define BIT_MANIPULATION_ASSUME(...) [[assume(__VA_ARGS__)]]
#elif defined(__clang__)
#define BIT_MANIPULATION_ASSUME(...) __builtin_assume(__VA_ARGS__)
#else
#define BIT_MANIPULATION_ASSUME(...)
#endif

} // namespace bit_manipulation

#endif
