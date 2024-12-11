#ifndef BIT_MANIPULATION_SOURCE_POSITION_HPP
#define BIT_MANIPULATION_SOURCE_POSITION_HPP

#include <string_view>

#include "common/assert.hpp"
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

    [[nodiscard]] constexpr Local_Source_Position to_left(Size offset) const
    {
        BIT_MANIPULATION_ASSERT(column >= offset);
        BIT_MANIPULATION_ASSERT(begin >= offset);
        return { .line = line, .column = column - offset, .begin = begin - offset };
    }
};

/// Represents a position in a source file.
struct Local_Source_Span : Local_Source_Position {
    Size length;

    [[nodiscard]] friend constexpr auto operator<=>(Local_Source_Span, Local_Source_Span) = default;

    /// @brief Returns a span with the same properties except that the length is `l`.
    [[nodiscard]] constexpr Local_Source_Span with_length(Size l) const
    {
        return { Local_Source_Position { *this }, l };
    }

    /// @brief Returns a span on the same line and with the same length, shifted to the right
    /// by `offset` characters.
    [[nodiscard]] constexpr Local_Source_Span to_right(Size offset) const
    {
        return { { .line = line, .column = column + offset, .begin = begin + offset }, length };
    }

    /// @brief Returns a span on the same line and with the same length, shifted to the left by
    /// `offset` characters.
    /// The `offset` shall not be greater than `this->column` or `this->begin`
    /// (which would correspond to flowing off the beginning of the column or source).
    [[nodiscard]] constexpr Local_Source_Span to_left(Size offset) const
    {
        BIT_MANIPULATION_ASSERT(column >= offset);
        BIT_MANIPULATION_ASSERT(begin >= offset);
        return { { .line = line, .column = column - offset, .begin = begin - offset }, length };
    }

    [[nodiscard]] constexpr bool empty() const
    {
        return length == 0;
    }

    /// @brief Returns the one-past-the-end column.
    [[nodiscard]] constexpr Size end_column() const
    {
        return column + length;
    }

    /// @brief Returns the one-past-the-end position in the source.
    [[nodiscard]] constexpr Size end() const
    {
        return begin + length;
    }

    /// @brief Returns the one-past-the-end position as a `Local_Source_Position`.
    /// This position is assumed to be on the same line and one column past this span.
    [[nodiscard]] constexpr Local_Source_Position end_pos() const
    {
        return { .line = line, .column = column + length, .begin = begin + length };
    }
};

/// Represents the location of a file, combined with the position within that file.
struct Source_Span : Local_Source_Span {
    /// File name.
    std::string_view file_name;

    [[nodiscard]] constexpr Source_Span(Local_Source_Span local, std::string_view file)
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

    [[nodiscard]] constexpr Source_Position(const Source_Span& span)
        : Local_Source_Position(span)
        , file_name(span.file_name)
    {
    }

    [[nodiscard]] constexpr Source_Position(Local_Source_Position local, std::string_view file)
        : Local_Source_Position(local)
        , file_name(file)
    {
    }

    [[nodiscard]] friend constexpr auto operator<=>(Source_Position, Source_Position) = default;
};

} // namespace bit_manipulation

#endif
