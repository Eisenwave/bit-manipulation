#include <charconv>

#include "common/assert.hpp"
#include "common/parse.hpp"

namespace bit_manipulation {

namespace {

constexpr bool are_hex_digits_ascii
    = 'a' + 1 == 'b' && 'b' + 1 == 'c' && 'c' + 1 == 'd' && 'd' + 1 == 'e' && 'e' + 1 == 'f';
static_assert(are_hex_digits_ascii);

constexpr bool safe_mul(Big_Uint& x, Big_Uint y)
{
    auto old = x;
    x *= y;
    return x >= old;
}

constexpr bool safe_add(Big_Uint& x, Big_Uint y)
{
    auto old = x;
    x += y;
    return x >= old;
}

/// @brief Parses a given string under the assumption that `str` is a sequence of digits which are
/// correct for the given `base`.
/// @param str the digit sequence, without any prefixes
/// @param base the base; shall be one of `2`, `8`, `10`, or `16`
/// @return the parsed integer value, or `std::nullopt` if it is not representable in the result
constexpr std::optional<Big_Uint> parse_uinteger_digits(std::string_view str, Big_Uint base)
{
    BIT_MANIPULATION_ASSERT(base >= 2);
    BIT_MANIPULATION_ASSERT(base <= 10 || base == 16);

    if (str.empty()) {
        return {};
    }

    Big_Uint result = 0;
    // We cannot simply use std::from_chars because Big_Uint might not be supported.

    if (base <= 10) {
        for (char c : str) {
            if (c < '0' || c > '9') {
                return {};
            }
            if (!safe_mul(result, base)) {
                return {};
            }
            if (!safe_add(result, Big_Uint(c - '0'))) {
                return {};
            }
        }
        return result;
    }

    BIT_MANIPULATION_ASSERT(base == 16);
    for (char c : str) {
        if (!safe_mul(result, 16)) {
            return {};
        }
        if (c >= '0' && c <= '9') {
            if (!safe_add(result, Big_Uint(c - '0'))) {
                return {};
            }
        }
        else if (c >= 'a' && c <= 'f') {
            if (!safe_add(result, Big_Uint(10 + c - 'a'))) {
                return {};
            }
        }
        else {
            return {};
        }
    }

    return result;
}

static_assert(*parse_uinteger_digits("10110", 2) == 0b10110);
static_assert(*parse_uinteger_digits("10157", 8) == 010157);
static_assert(*parse_uinteger_digits("123", 10) == 123);
static_assert(*parse_uinteger_digits("0", 16) == 0);
static_assert(*parse_uinteger_digits("ff", 16) == 255);

} // namespace

std::optional<Text_Match> match_line_comment(std::string_view s) noexcept
{
    if (!s.starts_with("//")) {
        return {};
    }
    const Size end = s.find('\n', 2);
    if (end == std::string_view::npos) {
        return Text_Match { .length = s.length(), .is_terminated = false };
    }
    return Text_Match { .length = end, .is_terminated = true };
}

std::optional<Text_Match> match_block_comment(std::string_view s) noexcept
{
    if (!s.starts_with("/*")) {
        return {};
    }
    // naive: nesting disallowed, but line comments can be nested in block comments
    const Size end = s.find("*/", 2);
    if (end == std::string_view::npos) {
        return Text_Match { .length = s.length(), .is_terminated = false };
    }
    return Text_Match { .length = end + 2, .is_terminated = true };
}

[[nodiscard]] std::optional<Text_Match> match_string_literal(std::string_view s) noexcept
{
    if (!s.starts_with('"')) {
        return {};
    }
    bool escaped = false;
    for (Size i = 1; i < s.size(); ++i) {
        if (escaped) {
            escaped = false;
        }
        else if (s[i] == '\\') {
            escaped = true;
        }
        else if (s[i] == '"') {
            return Text_Match { .length = i + 1, .is_terminated = true };
        }
    }
    return Text_Match { .length = s.length(), .is_terminated = false };
}

Size match_digits(std::string_view str, int base)
{
    BIT_MANIPULATION_ASSERT((base >= 2 && base <= 10) || base == 16);
    static constexpr std::string_view hexadecimal_digits = "0123456789abcdefABCDEF";

    const std::string_view digits
        = base == 16 ? hexadecimal_digits : hexadecimal_digits.substr(0, Size(base));

    // std::min covers the case of std::string_view::npos
    return std::min(str.find_first_not_of(digits), str.size());
}

Size match_identifier(std::string_view str) noexcept
{
    if (str.empty() || is_decimal_digit(str[0])) {
        return 0;
    }
    const Size result = str.find_first_not_of(identifier_characters);
    return std::min(result, str.length());
}

Literal_Match_Result match_integer_literal(std::string_view s) noexcept
{
    if (s.empty() || !is_decimal_digit(s[0])) {
        return { Literal_Match_Status::no_digits, 0, {} };
    }
    if (s.starts_with("0b")) {
        const Size digits = match_digits(s.substr(2), 2);
        if (digits == 0) {
            return { Literal_Match_Status::no_digits_following_prefix, 2, Literal_Type::binary };
        }
        return { Literal_Match_Status::ok, digits + 2, Literal_Type::binary };
    }
    if (s.starts_with("0x")) {
        const Size digits = match_digits(s.substr(2), 16);
        if (digits == 0) {
            return { Literal_Match_Status::no_digits_following_prefix, 2,
                     Literal_Type::hexadecimal };
        }
        return { Literal_Match_Status::ok, digits + 2, Literal_Type::hexadecimal };
    }
    if (s[0] == '0') {
        const Size digits = match_digits(s, 8);
        return { Literal_Match_Status::ok, digits,
                 digits == 1 ? Literal_Type::decimal : Literal_Type::octal };
    }
    const Size digits = match_digits(s, 10);

    return { Literal_Match_Status::ok, digits, Literal_Type::decimal };
}

std::optional<Big_Uint> parse_uinteger_literal(std::string_view str) noexcept
{
    if (str.empty()) {
        return std::nullopt;
    }
    if (str.starts_with("0b")) {
        return parse_uinteger_digits(str.substr(2), 2);
    }
    if (str.starts_with("0x")) {
        return parse_uinteger_digits(str.substr(2), 16);
    }
    if (str.starts_with("0")) {
        return parse_uinteger_digits(str, 8);
    }
    return parse_uinteger_digits(str, 10);
}

std::optional<Big_Int> parse_integer_literal(std::string_view str) noexcept
{
    if (str.empty()) {
        return std::nullopt;
    }
    if (str.starts_with("-")) {
        if (auto positive = parse_uinteger_literal(str.substr(1))) {
            // Negating as Uint is intentional and prevents overflow.
            return static_cast<Big_Int>(-*positive);
        }
        return std::nullopt;
    }
    if (auto result = parse_uinteger_literal(str)) {
        return static_cast<Big_Int>(*result);
    }
    return std::nullopt;
}

} // namespace bit_manipulation
