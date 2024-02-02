#include <vector>

#include "bms/fwd.hpp"
#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {
namespace ins {

/// @brief Loads a value from `source` and pushes it onto the stack.
struct Load {
    ast::Node_Handle source;
};

/// @brief Pushes `value` onto the stack.
struct Push {
    Concrete_Value value;
};

/// @brief Pops a value off the stack and stores it in `target`.
struct Store {
    ast::Node_Handle target;
};

/// @brief Jumps to the local instruction at index `label`.
/// Halts if `label` is invalid.
struct Jump {
    Size label;
};

/// @brief Pops a boolean value off the stack and if it equals `expected`,
/// jumps to the local instruction at index `label`.
struct Jump_If {
    Size label;
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
    ast::Node_Handle function;
};

} // namespace ins

enum struct Instruction_Type {
    load,
    push,
    store,
    jump,
    jump_if,
    jump_break,
    jump_continue,
    ret,
    unary_operate,
    binary_operate,
    call,
};

struct Instruction {
    Instruction_Type type;
    union {
        ins::Load load;
        ins::Push push;
        ins::Store store;
        ins::Jump jump;
        ins::Jump_If jump_if;
        ins::Break jump_break;
        ins::Continue jump_continue;
        ins::Return ret;
        ins::Unary_Operate unary_operate;
        ins::Binary_Operate binary_operate;
        ins::Call call;
    };
};

// A plain union is possible and relatively safe here because all instructions are trivially
// copyable and implicit-lifetime types.
static_assert(std::is_trivially_copyable_v<Instruction>);

} // namespace bit_manipulation::bms