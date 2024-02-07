#ifndef BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP
#define BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP

#include <variant>
#include <vector>

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {
namespace ins {

/// @brief Loads a value from `source` and pushes it onto the stack.
struct Load {
    ast::Handle source;
};

/// @brief Pushes `value` onto the stack.
struct Push {
    Concrete_Value value;
};

/// @brief Pops a value off the stack and stores it in `target`.
struct Store {
    ast::Handle target;
};

/// @brief Jumps to the local instruction at index `current + offset + 1`.
/// At `offset == 0`, this is a no-op instruction.
struct Relative_Jump {
    Signed_Size offset;
};

/// @brief Pops a boolean value off the stack and if it equals `expected`,
/// jumps to the local instruction at index `label`.
struct Relative_Jump_If {
    Signed_Size offset;
    bool expected;
};

/// @brief Symbolic jump to one past the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Break { };

/// @brief Symbolic jump to the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Continue { };

/// @brief Pops the return address off the stack and jumps the given address.
struct Return { };

/// @brief Pops a value off the stack, applies a unary operation, and pushes the result.
struct Unary_Operate {
    Token_Type op;
};

/// @brief Pops `y` off the stack. Pops `x` off the stack. Pushes `x op y`.
struct Binary_Operate {
    Token_Type op;
};

/// @brief Pushes the return address.
// Jumps to the called function.
struct Call {
    Size address;
};

} // namespace ins

using Instruction = std::variant<ins::Load,
                                 ins::Push,
                                 ins::Store,
                                 ins::Relative_Jump,
                                 ins::Relative_Jump_If,
                                 ins::Break,
                                 ins::Continue,
                                 ins::Return,
                                 ins::Unary_Operate,
                                 ins::Binary_Operate,
                                 ins::Call>;

// A plain union is possible and relatively safe here because all instructions are trivially
// copyable and implicit-lifetime types.
static_assert(std::is_trivially_copyable_v<Instruction>);

} // namespace bit_manipulation::bms

#endif