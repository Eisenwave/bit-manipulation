#include <gtest/gtest.h>

#include "common/to_chars.hpp"

namespace bit_manipulation {
namespace {

TEST(To_Chars, small_positive)
{
    for (int x = 0; x <= 9; ++x) {
        const char c = char(x + '0');
        std::string_view expected { &c, 1 };

        Characters chars_signed = to_characters(signed(x));
        EXPECT_EQ(chars_signed.as_string(), expected);

        Characters chars_unsigned = to_characters(unsigned(x));
        EXPECT_EQ(chars_unsigned.as_string(), expected);

#ifdef BIT_MANIPULATION_HAS_INT_128
        Characters chars_int128 = to_characters(Int128(x));
        EXPECT_EQ(chars_int128.as_string(), expected);

        Characters chars_uint128 = to_characters(Uint128(x));
        EXPECT_EQ(chars_uint128.as_string(), expected);
#endif
    }
}

TEST(To_Chars, small_negative)
{
    for (int x = 1; x <= 9; ++x) {
        const char c[] = { '-', char(x + '0') };
        std::string_view expected { c, 2 };
        Characters chars_int = to_characters(signed(-x));
        EXPECT_EQ(chars_int.as_string(), expected);

#ifdef BIT_MANIPULATION_HAS_INT_128
        Characters chars_128 = to_characters(Int128(-x));
        EXPECT_EQ(chars_128.as_string(), expected);
#endif
    }
}

#ifdef BIT_MANIPULATION_HAS_INT_128
template <typename T>
T power_ten(int x)
{
    T result = 1;
    for (int i = 0; i < x; ++i) {
        result *= 10;
    }
    return result;
}

TEST(To_Chars, huge_int128)
{
    Characters chars_min = to_characters(std::numeric_limits<Int128>::min());
    EXPECT_EQ(chars_min.as_string(), "-170141183460469231731687303715884105728");

    Characters chars_max = to_characters(std::numeric_limits<Int128>::max());
    EXPECT_EQ(chars_max.as_string(), "170141183460469231731687303715884105727");
}

TEST(To_Chars, huge_uint128)
{
    Characters chars = to_characters(std::numeric_limits<Uint128>::max());
    EXPECT_EQ(chars.as_string(), "340282366920938463463374607431768211455");
}

TEST(To_Chars, huge_pow10)
{
    EXPECT_EQ(to_characters(-power_ten<Int128>(20)).as_string(), //
              "-100000000000000000000");
    EXPECT_EQ(to_characters(power_ten<Int128>(20)).as_string(), //
              "100000000000000000000");
    EXPECT_EQ(to_characters(power_ten<Int128>(20) + 100).as_string(), //
              "100000000000000000100");

    EXPECT_EQ(to_characters(-power_ten<Int128>(38)).as_string(), //
              "-100000000000000000000000000000000000000");
    EXPECT_EQ(to_characters(power_ten<Int128>(38)).as_string(), //
              "100000000000000000000000000000000000000");
    EXPECT_EQ(to_characters(power_ten<Int128>(38) + 100).as_string(), //
              "100000000000000000000000000000000000100");
}
#endif

} // namespace
} // namespace bit_manipulation
