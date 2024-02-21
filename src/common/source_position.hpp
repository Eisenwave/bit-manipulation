#ifndef BIT_MANIPULATION_SOURCE_POSITION_HPP
#define BIT_MANIPULATION_SOURCE_POSITION_HPP

#include <string_view>

#include "common/config.hpp"

namespace bit_manipulation {

/// Represents a position in a source file.
struct Local_Source_Position {
    /// Line number.
    Size line;
    /// Column number.
    Size column;
    /// First index in the source file that is part of the syntactical element.
    Size begin;

    [[nodiscard]] friend constexpr auto operator<=>(Local_Source_Position, Local_Source_Position)
        = default;

    [[nodiscard]] constexpr Local_Source_Position to_right(Size offset) const
    {
        return { .line = line, .column = column + offset, .begin = begin + offset };
    }
};

/// Represents a position in a source file.
struct Local_Source_Span : Local_Source_Position {
    Size length;

    [[nodiscard]] friend constexpr auto operator<=>(Local_Source_Span, Local_Source_Span) = default;

    [[nodiscard]] constexpr Local_Source_Span to_right(Size offset) const
    {
        return { { .line = line, .column = column + offset, .begin = begin + offset }, length };
    }

    [[nodiscard]] constexpr Size end() const
    {
        return begin + length;
    }
};

/// Represents the location of a file, combined with the position within that file.
struct Source_Span : Local_Source_Span {
    /// File name.
    std::string_view file_name;

    [[nodiscard]] Source_Span(Local_Source_Span local, std::string_view file)
        : Local_Source_Span(local)
        , file_name(file)
    {
    }

    [[nodiscard]] friend constexpr auto operator<=>(Source_Span, Source_Span) = default;

    [[nodiscard]] constexpr Size end() const
    {
        return begin + length;
    }
};

/// Represents the location of a file, combined with the position within that file.
struct Source_Position : Local_Source_Position {
    /// File name.
    std::string_view file_name;

    [[nodiscard]] Source_Position(const Source_Span& span)
        : Local_Source_Position(span)
        , file_name(span.file_name)
    {
    }

    [[nodiscard]] Source_Position(Local_Source_Position local, std::string_view file)
        : Local_Source_Position(local)
        , file_name(file)
    {
    }

    [[nodiscard]] friend constexpr auto operator<=>(Source_Position, Source_Position) = default;
};

} // namespace bit_manipulation

#endif