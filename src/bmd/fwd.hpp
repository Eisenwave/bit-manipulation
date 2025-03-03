#ifndef BIT_MANIPULATION_BMD_FWD_HPP
#define BIT_MANIPULATION_BMD_FWD_HPP

#include "common/fwd.hpp"

namespace bit_manipulation::bmd {

enum struct Builtin_Directive_Type : Default_Underlying;

enum struct Code_Language : Default_Underlying;

enum struct Parse_Error_Code : Default_Underlying;
enum struct Document_Error_Code : Default_Underlying;
enum struct Generator_Error_Code : Default_Underlying;

struct Document_Error;
struct Generator_Error;

struct Directive_Type;
struct Parsed_Document;

struct HTML_Writer;

namespace ast {

struct Text;
struct Directive;
struct Argument;

struct Content;

} // namespace ast

} // namespace bit_manipulation::bmd

#endif
