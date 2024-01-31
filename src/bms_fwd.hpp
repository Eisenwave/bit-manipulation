#ifndef BIT_MANIPULATION_BMS_FWD_HPP
#define BIT_MANIPULATION_BMS_FWD_HPP

#include "config.hpp"

namespace bit_manipulation {

namespace ast {

/// A type which represents a handle into the AST.
/// It can be used only in conjunction with `Parsed_Program`.
/// This is basically just an index, but with more type safety and protection against misuse.
/// By only giving the user an index in the AST, it's possible to store it as a `std::vector` and
/// massively reduce the amount of allocations necessary.
enum struct Node_Handle : Size {
    // The null handle, representing no node.
    null = std::numeric_limits<Size>::max()
};

} // namespace ast

struct Parsed_Program;

} // namespace bit_manipulation

#endif