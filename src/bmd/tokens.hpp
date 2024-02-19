#ifndef BIT_MANIPULATION_BMD_TOKENS_HPP
#define BIT_MANIPULATION_BMD_TOKENS_HPP

#include <string_view>

#include "config.hpp"

#include "source_position.hpp"

namespace bit_manipulation::bmd {

enum struct Token_Type : Default_Underlying {
    // abc
    word,
    // C99-style comment
    line_comment,
    // C89-style comment
    block_comment,
    // empty line
    empty_line,
    // {
    left_brace,
    // }
    right_brace,
    // [
    left_bracket,
    // ]
    right_bracket,
    // ,
    comma,
    // =
    equal,
};

struct Token {
    Local_Source_Span pos {};
    Token_Type type {};

    [[nodiscard]] Token() = default;

    [[nodiscard]] Token(Local_Source_Span pos, Token_Type type) noexcept
        : pos { pos }
        , type { type }
    {
    }

    friend constexpr auto operator<=>(Token, Token) = default;
};

} // namespace bit_manipulation::bmd

#endif