#ifndef BIT_MANIPULATION_SOURCE_POSITION_HPP
#define BIT_MANIPULATION_SOURCE_POSITION_HPP

#include <string_view>

#include "config.hpp"

namespace bit_manipulation {

/// Represents a position in a source file.
struct Local_Source_Position {
    /// Line number.
    Size line;
    /// Column number.
    Size column;
    /// First index in the source file that is part of the syntactical element.
    Size begin;

    friend constexpr auto operator<=>(Local_Source_Position, Local_Source_Position) = default;
};

/// Represents a position in a source file.
struct Local_Source_Span : Local_Source_Position {
    Size length;

    friend constexpr auto operator<=>(Local_Source_Span, Local_Source_Span) = default;
};

/// Represents the location of a file, combined with the position within that file.
struct Source_Span : Local_Source_Span {
    /// File name.
    std::string_view file_name;

    Source_Span(Local_Source_Span local, std::string_view file)
        : Local_Source_Span(local)
        , file_name(file)
    {
    }

    friend constexpr auto operator<=>(Source_Span, Source_Span) = default;
};

/// Represents the location of a file, combined with the position within that file.
struct Source_Position : Local_Source_Position {
    /// File name.
    std::string_view file_name;

    Source_Position(const Source_Span& span)
        : Local_Source_Position(span)
        , file_name(span.file_name)
    {
    }

    Source_Position(Local_Source_Position local, std::string_view file)
        : Local_Source_Position(local)
        , file_name(file)
    {
    }

    friend constexpr auto operator<=>(Source_Position, Source_Position) = default;
};

} // namespace bit_manipulation

#endif