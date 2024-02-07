#ifndef BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP
#define BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP

#include <variant>
#include <vector>

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {
namespace ins {

namespace detail {
struct Base {
    /// @brief The AST node which emitted this instruction.
    ast::Handle debug_info;
};
} // namespace detail

/// @brief Loads a value from `source` and pushes it onto the stack.
struct Load : detail::Base {
    ast::Handle source;
};

/// @brief Pushes `value` onto the stack.
struct Push : detail::Base {
    Concrete_Value value;
};

/// @brief Pops a value off the stack and stores it in `target`.
struct Store : detail::Base {
    ast::Handle target;
};

/// @brief Jumps to the local instruction at index `current + offset + 1`.
/// At `offset == 0`, this is a no-op instruction.
struct Relative_Jump : detail::Base {
    Signed_Size offset;
};

/// @brief Pops a boolean value off the stack and if it equals `expected`,
/// jumps to the local instruction at index `label`.
struct Relative_Jump_If : detail::Base {
    Signed_Size offset;
    bool expected;
};

/// @brief Symbolic jump to one past the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Break : detail::Base { };

/// @brief Symbolic jump to the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Continue : detail::Base { };

/// @brief Pops the return address off the stack and jumps the given address.
struct Return : detail::Base { };

/// @brief Pops a value off the stack, applies a unary operation, and pushes the result.
struct Unary_Operate : detail::Base {
    Token_Type op;
};

/// @brief Pops `y` off the stack. Pops `x` off the stack. Pushes `x op y`.
struct Binary_Operate : detail::Base {
    Token_Type op;
};

/// @brief Pushes the return address.
// Jumps to the called function.
struct Call : detail::Base {
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