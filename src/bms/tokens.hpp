#ifndef BIT_MANIPULATION_BMS_TOKENS_HPP
#define BIT_MANIPULATION_BMS_TOKENS_HPP

#include <string_view>

#include "common/config.hpp"
#include "common/source_position.hpp"

#include "bms/token_type.hpp"

namespace bit_manipulation::bms {

struct Token {
    Local_Source_Span pos {};
    Token_Type type {};

    [[nodiscard]] Token() = default;

    [[nodiscard]] Token(const Local_Source_Span& pos, Token_Type type) noexcept
        : pos { pos }
        , type { type }
    {
    }

    friend constexpr auto operator<=>(Token, Token) = default;
};

} // namespace bit_manipulation::bms

#endif
