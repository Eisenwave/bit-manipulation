#ifndef BIT_MANIPULATION_BMS_FWD_HPP
#define BIT_MANIPULATION_BMS_FWD_HPP

#include "config.hpp"

namespace bit_manipulation::bms {

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

struct Program_Node;
struct Function_Node;

} // namespace ast

enum struct Token_Type : int;

enum struct Evaluation_Error : int;
enum struct Execution_Error_Code : int;
enum struct Type_Error_Code : int;

struct Parsed_Program;

struct Token;

/// Represents a position in a source file.
struct Source_Position {
    /// Line number.
    Size line;
    /// Column number.
    Size column;
    /// First index in the source file that is part of the syntactical element.
    Size begin;
};

} // namespace bit_manipulation::bms

#endif