#ifndef BIT_MANIPULATION_BMD_FWD_HPP
#define BIT_MANIPULATION_BMD_FWD_HPP

#include "common/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Parse_Error_Code : Default_Underlying;

struct Parse_Error;

struct Parsed_Program;

namespace ast {

struct Content;
struct Paragraph;
struct Text;
struct Number;
struct Directive;

struct Some_Node;

} // namespace ast

} // namespace bit_manipulation::bmd

#endif