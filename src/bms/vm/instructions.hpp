#ifndef BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP
#define BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP

#include <iosfwd>
#include <vector>

#include "common/variant.hpp"

#include "bms/concrete_value.hpp"
#include "bms/debug_info.hpp"
#include "bms/fwd.hpp"
#include "bms/tokenization/token_type.hpp"

namespace bit_manipulation::bms {
namespace ins {

namespace detail {
struct Base {
    Debug_Info debug_info;
};
} // namespace detail

/// @brief Loads a value from `source` and pushes it onto the stack.
struct Load : detail::Base {
    const void* source;
};

std::ostream& operator<<(std::ostream& out, const Load&);

/// @brief Pops a value off the stack and stores it in `target`.
struct Store : detail::Base {
    const void* target;
};

std::ostream& operator<<(std::ostream& out, const Store&);

/// @brief Pushes `value` onto the stack.
struct Push : detail::Base {
    Concrete_Value value;
};

std::ostream& operator<<(std::ostream& out, const Push&);

/// @brief Pops a value off the stack and discards it.
struct Pop : detail::Base { };

std::ostream& operator<<(std::ostream& out, const Pop&);

/// @brief Jumps to the local instruction at index `current + offset + 1`.
/// At `offset == 0`, this is a no-op instruction.
struct Relative_Jump : detail::Base {
    Signed_Size offset;
};

std::ostream& operator<<(std::ostream& out, const Relative_Jump&);

/// @brief Pops a boolean value off the stack and if it equals `expected`,
/// jumps to the local instruction at index `label`.
struct Relative_Jump_If : detail::Base {
    Signed_Size offset;
    bool expected;
};

std::ostream& operator<<(std::ostream& out, const Relative_Jump_If&);

/// @brief Symbolic jump to one past the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Break : detail::Base { };

std::ostream& operator<<(std::ostream& out, const Break&);

/// @brief Symbolic jump to the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Continue : detail::Base { };

std::ostream& operator<<(std::ostream& out, const Continue&);

/// @brief Pops the return address off the stack and jumps the given address.
struct Return : detail::Base { };

std::ostream& operator<<(std::ostream& out, const Return&);

/// @brief Pops a value off the stack, converts it to the target type, and pushes the result.
struct Convert : detail::Base {
    Concrete_Type type;
};

std::ostream& operator<<(std::ostream& out, const Convert&);

/// @brief Pops a value off the stack, applies a unary operation, and pushes the result.
struct Unary_Operate : detail::Base {
    Expression_Type op;
};

std::ostream& operator<<(std::ostream& out, const Unary_Operate&);

/// @brief Pops `y` off the stack. Pops `x` off the stack. Pushes `x op y`.
struct Binary_Operate : detail::Base {
    Expression_Type op;
};

std::ostream& operator<<(std::ostream& out, const Binary_Operate&);

/// @brief Pushes the return address.
// Jumps to the called function.
struct Call : detail::Base {
    Size address;
};

std::ostream& operator<<(std::ostream& out, const Call&);

/// @brief Pops the required parameters off the stack and calls the builtin function.
struct Builtin_Call : detail::Base {
    Builtin_Function function;
};

std::ostream& operator<<(std::ostream& out, const Builtin_Call&);

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

std::ostream& operator<<(std::ostream&, const Instruction&);

void dump_program(std::ostream&, std::span<const Instruction>);

namespace detail {

inline const ins::detail::Base& to_base(const Instruction& i)
{
    return visit([](auto& x) -> const ins::detail::Base& { return x; }, i);
}

} // namespace detail

inline Debug_Info get_debug_info(const Instruction& i)
{
    return detail::to_base(i).debug_info;
}

} // namespace bit_manipulation::bms

#endif
