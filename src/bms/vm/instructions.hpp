#ifndef BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP
#define BIT_MANIPULATION_BMS_VM_INSTRUCTIONS_HPP

#include <span>

#include "common/function_ref.hpp"
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
    // TODO: for the purpose of testability, come up with a more portable representation than
    //       to store pointers to AST nodes (which makes it difficult to create test expectations)
    const void* source;

    [[nodiscard]] friend bool operator==(const Load& x, const Load& y) noexcept
    {
        return x.source == y.source;
    }
};

/// @brief Pops a value off the stack and stores it in `target`.
struct Store : detail::Base {
    const void* target;

    [[nodiscard]] friend bool operator==(const Store& x, const Store& y) noexcept
    {
        return x.target == y.target;
    }
};

/// @brief Pushes `value` onto the stack.
struct Push : detail::Base {
    Concrete_Value value;

    [[nodiscard]] friend bool operator==(const Push& x, const Push& y) noexcept
    {
        return x.value == y.value;
    }
};

/// @brief Pops a value off the stack and discards it.
struct Pop : detail::Base {
    [[nodiscard]] friend bool operator==(const Pop&, const Pop&) noexcept
    {
        return true;
    }
};

/// @brief Jumps to the local instruction at index `current + offset + 1`.
/// At `offset == 0`, this is a no-op instruction.
struct Relative_Jump : detail::Base {
    Signed_Size offset;

    [[nodiscard]] friend bool operator==(const Relative_Jump& x, const Relative_Jump& y) noexcept
    {
        return x.offset == y.offset;
    }
};

/// @brief Pops a boolean value off the stack and if it equals `expected`,
/// jumps to the local instruction at index `label`.
struct Relative_Jump_If : detail::Base {
    Signed_Size offset;
    bool expected;

    [[nodiscard]] friend bool operator==(const Relative_Jump_If& x,
                                         const Relative_Jump_If& y) noexcept
    {
        return x.offset == y.offset && x.expected == y.expected;
    }
};

/// @brief Symbolic jump to one past the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Break : detail::Base {
    [[nodiscard]] friend bool operator==(const Break&, const Break&) noexcept
    {
        return true;
    }
};

/// @brief Symbolic jump to the end of a loop.
/// This must be converted to a `Jump` prior to evaluation.
struct Continue : detail::Base {
    [[nodiscard]] friend bool operator==(const Continue&, const Continue&) noexcept
    {
        return true;
    }
};

/// @brief Pops the return address off the stack and jumps the given address.
struct Return : detail::Base {
    [[nodiscard]] friend bool operator==(const Return&, const Return&) noexcept
    {
        return true;
    }
};

/// @brief Pops a value off the stack, converts it to the target type, and pushes the result.
struct Convert : detail::Base {
    Concrete_Type type;

    [[nodiscard]] friend bool operator==(const Convert& x, const Convert& y) noexcept
    {
        return x.type == y.type;
    }
};

/// @brief Pops a value off the stack, applies a unary operation, and pushes the result.
struct Unary_Operate : detail::Base {
    Expression_Type op;

    [[nodiscard]] friend bool operator==(const Unary_Operate& x, const Unary_Operate& y) noexcept
    {
        return x.op == y.op;
    }
};

/// @brief Pops `y` off the stack. Pops `x` off the stack. Pushes `x op y`.
struct Binary_Operate : detail::Base {
    Expression_Type op;

    [[nodiscard]] friend bool operator==(const Binary_Operate& x, const Binary_Operate& y) noexcept
    {
        return x.op == y.op;
    }
};

/// @brief Pushes the return address.
// Jumps to the called function.
struct Call : detail::Base {
    Size address;

    [[nodiscard]] friend bool operator==(const Call& x, const Call& y) noexcept
    {
        return x.address == y.address;
    }
};

/// @brief Symbolic function call, to be turned into `Call` later on.
struct Symbolic_Call : detail::Base {
    const ast::Function* target;

    [[nodiscard]] friend bool operator==(const Symbolic_Call& x, const Symbolic_Call& y) noexcept
    {
        return x.target == y.target;
    }
};

/// @brief Pops the required parameters off the stack and calls the builtin function.
struct Builtin_Call : detail::Base {
    Builtin_Function function;

    [[nodiscard]] friend bool operator==(const Builtin_Call& x, const Builtin_Call& y) noexcept
    {
        return x.function == y.function;
    }
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
                                    ins::Symbolic_Call,
                                    ins::Builtin_Call>;

struct Instruction : Instruction_Variant {
    using Variant::Variant;
};

struct Program_Print_Options {
    /// @brief The indentation width in spaces.
    int indent = 4;
    /// @brief If `true`, doesn't use debug info (such as for printing additional helpful comments).
    /// This may be necessary when printing the program when the AST is no longer alive.
    bool ignore_debug_info = false;
};

/// @brief Prints a program, consisting of a span of instructions.
/// Also potentially prints labels, if a `print_label` function is provided.
/// @param out the string to print to
/// @param instructions the span of instructions
/// @param print_label invoked for each instruction `index`,
/// with the given `out` string and the `index`;
/// if there is a label at that index, `print_label` should write its contents to `out` and
/// return `false`, otherwise write nothing and return `false`.
/// The label should not include a trailing newline character or colon;
/// such formatting is appended automatically.
void print_program(Code_String& out,
                   std::span<const Instruction> instructions,
                   Program_Print_Options options = {},
                   Function_Ref<bool(Code_String& out, Size index)> print_label = {});

struct Function_Print_Options {
    bool name : 1 = true;
    bool parameters : 1 = false;
    bool return_type : 1 = false;
    bool whitespace : 1 = false;
};

/// @brief Prints a label based on a function.
/// @param out the output string
/// @param f the function
/// @param options additional options
void print_function_label(Code_String& out, const ast::Function& f, Function_Print_Options options);

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
