#ifndef BIT_MANIPULATION_BMS_PRINT_HPP
#define BIT_MANIPULATION_BMS_PRINT_HPP

#include "common/code_span_type.hpp"
#include "common/code_string.hpp"

#include "bms/concrete_type.hpp"
#include "bms/concrete_value.hpp"

namespace bit_manipulation::bms {

inline void print_type(Code_String& out, const Concrete_Type& type)
{
    out.append(type_type_name(type.type()), Code_Span_Type::type_name);
    if (type.type() == Type_Type::Uint) {
        out.append('(', Code_Span_Type::punctuation);
        out.append_integer(type.width(), Code_Span_Type::number);
        out.append(')', Code_Span_Type::punctuation);
    }
}

inline void print_bool(Code_String& out, bool value)
{
    out.append(value ? "true" : "false", Code_Span_Type::boolean_literal);
}

inline void print_value(Code_String& out, const Concrete_Value& value)
{
    switch (Type_Type type = value.get_type().type()) {
    case Type_Type::Nothing:
    case Type_Type::Void: //
        out.append(type_type_name(type), Code_Span_Type::type_name);
        return;
    case Type_Type::Bool: //
        print_bool(out, value.as_int());
        return;
    case Type_Type::Int: //
        out.append_integer(value.as_int(), Code_Span_Type::number);
        return;
    case Type_Type::Uint: //
        out.append_integer(value.as_uint(), Code_Span_Type::number);
        return;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("value has unknown type.");
}

} // namespace bit_manipulation::bms

#endif
