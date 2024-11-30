#ifndef BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP
#define BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP

#include <vector>

#include "bms/concrete_value.hpp"
#include "bms/fwd.hpp"
#include "bms/tokenization/token_type.hpp"

namespace bit_manipulation::bms {
namespace ins {

namespace detail {
struct Base {
    /// @brief The AST node which emitted this instruction.
    const ast::Some_Node* debug_info;
};
} // namespace detail

/// @brief Loads a value from `source` and pushes it onto the stack.
struct Load : detail::Base {
    const ast::Some_Node* source;
};

/// @brief Pops a value off the stack and stores it in `target`.
struct Store : detail::Base {
    const ast::Some_Node* target;
};

/// @brief Pushes `value` onto the stack.
struct Push : detail::Base {
    Concrete_Value value;
};

/// @brief Pops a value off the stack and discards it.
struct Pop : detail::Base { };

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

/// @brief Pops a value off the stack, converts it to the target type, and pushes the result.
struct Convert : detail::Base {
    Concrete_Type type;
};

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

/// @brief Pops the required parameters off the stack and calls the builtin function.
struct Builtin_Call : detail::Base {
    Builtin_Function function;
};

} // namespace ins

using Instruction_Variant = Variant<ins::Load,
                                    ins::Store,
                                    ins::Push,
                                    ins::Pop,
                                    ins::Relative_Jump,
                                    ins::Relative_Jump_If,
                                    ins::Break,
                                    ins::Continue,
                                    ins::Return,
                                    ins::Convert,
                                    ins::Unary_Operate,
                                    ins::Binary_Operate,
                                    ins::Call,
                                    ins::Builtin_Call>;

struct Instruction : Instruction_Variant {
    using Variant::Variant;
};

namespace detail {

inline const ins::detail::Base& to_base(const Instruction& i)
{
    return visit([](auto& x) -> const ins::detail::Base& { return x; }, i);
}

} // namespace detail

inline const ast::Some_Node* get_debug_info(const Instruction& i)
{
    return detail::to_base(i).debug_info;
}

} // namespace bit_manipulation::bms

#endif
