#ifndef BIT_MANIPULATION_BMS_ANALYSIS_LEVEL_HPP
#define BIT_MANIPULATION_BMS_ANALYSIS_LEVEL_HPP

#include <compare>

#include "common/config.hpp"

namespace bit_manipulation {

// TODO: this doesn't really belong here, or the header should be renamed

/// @brief The level of analysis to perform on an AST node that represents a definable entity,
/// such as a function.
/// @see ast::Function::analysis_so_far
enum struct Analysis_Level : Default_Underlying {
    /// @brief No analysis has taken place.
    unanalyzed,
    /// @brief Analysis only of the declaration, but not of the definition.
    /// For example, a function undergoes shallow analysis when it is called
    /// so that type checking (matching arguments to parameters) can be done.
    shallow,
    /// @brief Full analysis of a function, including its body.
    /// This is done when e.g. a function definition appears in code,
    /// and the contents of its body should be analyzed.
    full,
    /// @brief Full analysis, recursively, for analyzing functions called in constant expressions.
    /// In addition to the requirements posed by `full`,
    /// this requires a definition of any called function to be available, recursively.
    /// These requirements are extremely similar to requirements on `constexpr` functions.
    for_constant_evaluation
};

constexpr auto operator<=>(Analysis_Level x, Analysis_Level y) noexcept
{
    return static_cast<Default_Underlying>(x) <=> static_cast<Default_Underlying>(y);
}

} // namespace bit_manipulation

#endif
