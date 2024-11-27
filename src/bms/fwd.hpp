#ifndef BIT_MANIPULATION_BMS_FWD_HPP
#define BIT_MANIPULATION_BMS_FWD_HPP

#include <string_view>

#include "common/config.hpp"
#include "common/fwd.hpp"

namespace bit_manipulation::bms {

namespace astp {

namespace detail {
struct Node_Base;
}

/// A type which represents a handle into the AST.
/// It can be used only in conjunction with `Parsed_Program`.
/// This is basically just an index, but with more type safety and protection against misuse.
/// By only giving the user an index in the AST, it's possible to store it as a `std::vector` and
/// massively reduce the amount of allocations necessary.
enum struct Handle : Size {
    // The null handle, representing no node.
    null = std::numeric_limits<Size>::max()
};

struct Program;
struct Function;
struct Parameter_List;
struct Parameter;
struct Type;
struct Const;
struct Let;
struct Static_Assert;
struct If_Statement;
struct While_Statement;
struct Break;
struct Continue;
struct Return_Statement;
struct Assignment;
struct Block_Statement;
struct Conversion_Expression;
struct If_Expression;
struct Binary_Expression;
struct Prefix_Expression;
struct Function_Call_Expression;
struct Id_Expression;
struct Literal;

struct Some_Node;

} // namespace astp

namespace ast {

struct Program;
struct Function;
struct Parameter_List;
struct Parameter;
struct Type;
struct Const;
struct Let;
struct Static_Assert;
struct If_Statement;
struct While_Statement;
struct Break;
struct Continue;
struct Return_Statement;
struct Assignment;
struct Block_Statement;
struct Conversion_Expression;
struct If_Expression;
struct Binary_Expression;
struct Prefix_Expression;
struct Function_Call_Expression;
struct Id_Expression;
struct Literal;
struct Builtin_Function;

struct Some_Node;

} // namespace ast

namespace ins {

struct Load;
struct Store;
struct Push;
struct Pop;
struct Relative_Jump;
struct Relative_Jump_If;
struct Break;
struct Continue;
struct Return;
struct Convert;
struct Unary_Operate;
struct Binary_Operate;
struct Call;
struct Builtin_Call;

} // namespace ins

struct Instruction;

enum struct Token_Type : Default_Underlying;
enum struct Grammar_Rule : Default_Underlying;

enum struct Analysis_Error_Code : Default_Underlying;
enum struct Evaluation_Error_Code : Default_Underlying;
enum struct Execution_Error_Code : Default_Underlying;

enum struct Type_Type : Default_Underlying;
enum struct Builtin_Function : Default_Underlying;

struct Tokenize_Error;
struct Parse_Error;
struct Analysis_Error;
struct Execution_Error;

struct Parsed_Program;
struct Analyzed_Program;

struct Token;

} // namespace bit_manipulation::bms

#endif
