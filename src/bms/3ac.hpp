#include <vector>

#include "bms/fwd.hpp"
#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms::ins {

/// @brief Initializes a variable, identified by the AST node handle `var`.
/// This will not give it a value yet, only make it available for stores.
/// Halts if `var` is initialized.
struct Initialize {
    ast::Node_Handle var;
};

/// @brief Loads a value from `source` and stores it in register `target`.
/// Halts if `source` is not initialized.
struct Load {
    Size target;
    ast::Node_Handle source;
};

/// @brief Stores `value` in register `target`.
struct Load_Immediate {
    Size target;
    Value value;
};

/// @brief Stores the value in register `source` in the variable identified by the AST node
/// `target`.
/// Halts if `target` is not initialized.
struct Store {
    ast::Node_Handle target;
    Size source;
};

/// @brief Stores `value` in the variable identified by the AST node `target`.
/// Halts if `target` is not initialized.
struct Store_Immediate {
    ast::Node_Handle target;
    Value value;
};

/// @brief Jumps to the local instruction at index `label`.
/// Halts if `label` is invalid.
struct Jump {
    Size label;
};

/// @brief If the value of register `var` is `expected`,
/// jumps to the local instruction at index `label`.
/// Halts if `var` does not contain a `Bool`.
struct Jump_If {
    Size label;
    Size var;
    bool expected;
};

/// @brief Symbolic jump to one past the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Break { };

/// @brief Symbolic jump to the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Continue { };

/// @brief Places the value of register `var` in the return register and returns control to the
/// caller of the function.
struct Return {
    Size var;
};

/// @brief Performs a unary operation `op` with register `var` and stores the result in `target`.
struct Unary_Operate {
    Size target;
    Token_Type op;
    Size var;
};

/// @brief Performs a binary operation `op` with registers `left` and `right` and stores the result
/// in `target`.
struct Binary_Operate {
    Size target;
    Token_Type op;
    Size left;
    Size right;
};

/// @brief Pushes the value in register `var` onto the argument stack.
struct Push_Argument {
    Size var;
};

/// @brief Pushes a constant `value` onto the argument stack.
struct Push_Argument_Immediate {
    Value value;
};

/// @brief Calls a function, which consumes the argument stack.
/// Halts if the amount of argument is insufficient to perform the call.
/// Upon entry, a new stack frame is created for the function, and control is transferred to it.
/// Upon exit, the result of the function is stored in the result register.
struct Call {
    Size target;
    ast::Node_Handle function;
};

} // namespace bit_manipulation::bms::ins