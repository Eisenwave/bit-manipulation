#include <charconv>

#include "assert.hpp"

#include "parse.hpp"

namespace bit_manipulation {

namespace {

constexpr bool are_hex_digits_ascii
    = 'a' + 1 == 'b' && 'b' + 1 == 'c' && 'c' + 1 == 'd' && 'd' + 1 == 'e' && 'e' + 1 == 'f';
static_assert(are_hex_digits_ascii);

constexpr std::optional<Big_Uint> parse_uinteger_digits(std::string_view str, Big_Int base) noexcept
{
    BIT_MANIPULATION_ASSERT(base >= 2);
    BIT_MANIPULATION_ASSERT(base <= 10 || base == 16);

    if (str.empty()) {
        return std::nullopt;
    }

    Big_Int result = 0;
    // We cannot simply use std::from_chars because Big_Int might not be supported.

    if (base <= 10) {
        for (char c : str) {
            if (c < '0' || c > '9') {
                return std::nullopt;
            }
            result *= base;
            result += c - '0';
        }
        return result;
    }

    BIT_MANIPULATION_ASSERT(base == 16);
    for (char c : str) {
        result *= 16;
        if (c >= '0' && c <= '9') {
            result += c - '0';
        }
        else if (c >= 'a' && c <= 'f') {
            result += 10 + c - 'a';
        }
        else {
            return std::nullopt;
        }
    }

    return result;
}

static_assert(*parse_uinteger_digits("10110", 2) == 0b10110);
static_assert(*parse_uinteger_digits("10157", 8) == 010157);
static_assert(*parse_uinteger_digits("123", 10) == 123);
static_assert(*parse_uinteger_digits("ff", 16) == 255);

} // namespace

std::optional<Comment_Match> match_line_comment(std::string_view s) noexcept
{
    if (!s.starts_with("//")) {
        return std::nullopt;
    }
    const Size end = s.find('\n', 2);
    if (end == std::string_view::npos) {
        return Comment_Match { .length = s.length(), .is_terminated = false };
    }
    return Comment_Match { .length = end, .is_terminated = true };
}

std::optional<Comment_Match> match_block_comment(std::string_view s) noexcept
{
    if (!s.starts_with("/*")) {
        return std::nullopt;
    }
    // naive: nesting disallowed, but line comments can be nested in block comments
    const Size end = s.find("*/", 2);
    if (end == std::string_view::npos) {
        return Comment_Match { .length = s.length(), .is_terminated = false };
    }
    return Comment_Match { .length = end + 2, .is_terminated = true };
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
    // The if statement at the beginning of this function should have made sure that at least one
    // decimal digit exists.
    BIT_MANIPULATION_ASSERT(digits != 0);

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