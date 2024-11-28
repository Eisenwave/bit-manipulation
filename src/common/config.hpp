#ifndef BIT_MANIPULATION_CONFIG_HPP
#define BIT_MANIPULATION_CONFIG_HPP

#include <cstddef>
#include <cstdint>
#include <limits>
#include <type_traits>

#ifndef NDEBUG // debug builds
#define BIT_MANIPULATION_IF_DEBUG(...) __VA_ARGS__
#define BIT_MANIPULATION_IF_NOT_DEBUG(...)
#else // release builds
#define BIT_MANIPULATION_IF_DEBUG(...)
#define BIT_MANIPULATION_IF_NOT_DEBUG(...) __VA_ARGS__
#endif

#define BIT_MANIPULATION_UNREACHABLE() __builtin_unreachable()

namespace bit_manipulation {

using Byte = std::byte;

inline constexpr bool compute_lut = false;

#ifdef __GNUC__
#define BIT_MANIPULATION_HAS_INT_128 1
inline constexpr bool has_128_bit_integer = true;
/// @brief 128-bit unsigned integer.
__extension__ typedef unsigned __int128 Uint128;
/// @brief 128-bit signed integer.
__extension__ typedef signed __int128 Int128;
#else
inline constexpr bool has_128_bit_integer = false;
/// @brief 128-bit unsigned integer not supported.
using Uint128 = void;
/// @brief 128-bit signed integer not supported.
using Int128 = void;
#endif

/// @brief 64-bit unsigned integer.
using Uint64 = std::uint64_t;
/// @brief 64-bit signed integer.
using Int64 = std::int64_t;
/// @brief 32-bit unsigned integer.
using Uint32 = std::uint32_t;
/// @brief 32-bit signed integer.
using Int32 = std::int32_t;
/// @brief 16-bit unsigned integer.
using Uint16 = std::uint16_t;
/// @brief 16-bit signed integer.
using Int16 = std::int16_t;
/// @brief 8-bit unsigned integer.
using Uint8 = std::uint8_t;
/// @brief 8-bit signed integer.
using Int8 = std::int8_t;

/// @brief Convenience alias for std::size_t.
using Size = std::size_t;
/// @brief Signed counterpart to `uz`.
using Signed_Size = std::make_signed_t<Size>;
/// @brief Convenience alias for `std::ptrdiff_t`.
using Difference = std::ptrdiff_t;

/// @brief The integer type used throughout the program.
using Int = Int64;
/// @brief The unsigned integer used throughout the program.
/// Guaranteed to have the same width as `integer`.
using Uint = Uint64;

inline constexpr int uint_max_width = 128;

using Big_Int = Int128;
using Big_Uint = Uint128;

static_assert(sizeof(Int) == sizeof(Uint));

/// @brief The default underlying type for scoped enumerations.
using Default_Underlying = unsigned char;

#define BIT_MANIPULATION_ENUM_STRING_CASE(...)                                                     \
    case __VA_ARGS__: return #__VA_ARGS__

template <typename>
inline constexpr bool dependent_false = false;

} // namespace bit_manipulation

#endif
