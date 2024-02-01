#include <charconv>

#include "assert.hpp"

#include "bms/parse_number.hpp"

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