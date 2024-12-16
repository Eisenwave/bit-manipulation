#ifndef BIT_MANIPULATION_TO_STRING_HPP
#define BIT_MANIPULATION_TO_STRING_HPP

#include <charconv>
#include <concepts>
#include <limits>
#include <string>

#include "common/assert.hpp"
#include "common/config.hpp"

namespace bit_manipulation {

/// @brief Customization point object for character conversions.
/// Will simply forward to `std::to_chars` for most integers, and use
/// a custom implementation for 128-bit integers.
inline constexpr struct To_Chars {

    template <typename T>
        requires requires(char* p, T x) { std::to_chars(p, p, x); }
    [[nodiscard]] std::to_chars_result operator()(char* begin, char* end, T x) const
    {
        return std::to_chars(begin, end, x);
    }

#ifdef BIT_MANIPULATION_HAS_INT_128
    [[nodiscard]] std::to_chars_result operator()(char* begin, char* end, Int128 x) const;
    [[nodiscard]] std::to_chars_result operator()(char* begin, char* end, Uint128 x) const;
#endif

} to_chars;

template <typename T>
constexpr int approximate_to_chars_decimal_digits_v
    = (std::numeric_limits<T>::digits * 100 / 310) + 1 + std::is_signed_v<T>;

template <Size N>
struct Characters {
    std::array<char, N> buffer;
    Size length;

    [[nodiscard]] std::string_view as_string() const
    {
        return { buffer.data(), length };
    }
};

template <typename T>
concept digit_sequence = requires {
    std::numeric_limits<T>::digits;
    std::is_signed_v<T>;
};

template <typename T>
concept to_chars_able = requires(char* p, T x) { to_chars(p, p, x); };

template <typename T>
concept character_convertible = digit_sequence<T> && to_chars_able<T>;

template <character_convertible T>
[[nodiscard]] constexpr Characters<approximate_to_chars_decimal_digits_v<T>> to_characters(T x)
{
    Characters<approximate_to_chars_decimal_digits_v<T>> chars {};
    auto result = to_chars(chars.buffer.data(), chars.buffer.data() + chars.buffer.size(), x);
    BIT_MANIPULATION_ASSERT(result.ec == std::errc {});
    chars.length = Size(result.ptr - chars.buffer.data());
    return chars;
}

[[deprecated]] std::string to_string(Uint128 x);

[[deprecated]] std::string to_string(Int128 x);

} // namespace bit_manipulation

#endif
