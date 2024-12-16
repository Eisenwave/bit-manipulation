#include <cstring>

#include "common/to_chars.hpp"

namespace bit_manipulation {

static_assert(approximate_to_chars_decimal_digits_v<unsigned char> >= 3);
static_assert(approximate_to_chars_decimal_digits_v<unsigned short> >= 6);

static_assert(character_convertible<int>);
static_assert(character_convertible<unsigned>);
static_assert(character_convertible<Size>);
static_assert(character_convertible<Signed_Size>);
static_assert(character_convertible<Big_Int>);
static_assert(character_convertible<Big_Uint>);

#ifdef BIT_MANIPULATION_HAS_INT_128
std::to_chars_result To_Chars::operator()(char* begin, char* end, Int128 x) const
{
    if (x >= 0) {
        return (*this)(begin, end, Uint128(x));
    }
    if (x >= std::numeric_limits<Int64>::min()) [[likely]] {
        return std::to_chars(begin, end, Int64(x));
    }
    if (end - begin < 2) [[unlikely]] {
        return { begin, std::errc::value_too_large };
    }
    *begin = '-';
    return (*this)(begin + 1, end, -Uint128(x));
}

std::to_chars_result To_Chars::operator()(char* begin, char* end, Uint128 x) const
{
    if (x <= std::uint64_t(-1)) [[likely]] {
        return std::to_chars(begin, end, Uint64(x));
    }

    /// The greatest power of 10 that fits into a 64-bit integer.
    constexpr Uint128 exp10_19 = 10000000000000000000ull;
    constexpr int lower_max_digits = 19;

    auto upper_result = (*this)(begin, end, x / exp10_19);
    if (upper_result.ec != std::errc {}) {
        return upper_result;
    }

    auto lower_result = std::to_chars(upper_result.ptr, end, Uint64(x % exp10_19));
    if (lower_result.ec != std::errc {}) {
        return lower_result;
    }
    const Difference lower_length = lower_result.ptr - upper_result.ptr;

    char* result_end = upper_result.ptr + lower_max_digits;
    char* result_begin = result_end - lower_length;
    std::memmove(result_begin, upper_result.ptr, Size(lower_length));
    std::memset(upper_result.ptr, '0', Size(lower_max_digits - lower_length));

    return { result_end, std::errc {} };
}
#endif

} // namespace bit_manipulation
