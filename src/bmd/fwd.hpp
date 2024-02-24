#ifndef BIT_MANIPULATION_BMD_FWD_HPP
#define BIT_MANIPULATION_BMD_FWD_HPP

#include "common/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Directive_Type : Default_Underlying;
enum struct Formatting_Style : Default_Underlying;

enum struct Parse_Error_Code : Default_Underlying;

struct Parse_Error;

struct Parsed_Document;

struct HTML_Writer;
struct HTML_Token_Consumer;

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
