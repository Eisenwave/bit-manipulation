#include <iomanip>
#include <ostream>

#include "common/to_string.hpp"

#include "bms/evaluation/builtin_function.hpp"
#include "bms/expression_type.hpp"
#include "bms/vm/instructions.hpp"

namespace bit_manipulation::bms {

std::ostream& operator<<(std::ostream& out, const Concrete_Type& type)
{
    out << type_type_name(type.type());

    if (type.type() == Type_Type::Uint) {
        out << '(' << type.width() << ')';
    }

    return out;
}

std::ostream& operator<<(std::ostream& out, const Concrete_Value& value)
{
    switch (value.type.type()) {
    case Type_Type::Void: return out << "Void";
    case Type_Type::Bool: return out << (value.int_value ? "true" : "false");
    case Type_Type::Int: return out << to_string(value.int_value);
    case Type_Type::Uint: return out << to_string(Big_Uint(value.int_value));
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("value has unknown type.");
}

namespace ins {

constexpr int name_column_width = 16;

std::ostream& operator<<(std::ostream& out, const Load& i)
{
    out << std::left << std::setw(name_column_width) << "load " << i.source;
    if (!i.debug_info.name.empty()) {
        out << "    ; " << i.debug_info.name;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const Store& i)
{
    out << std::left << std::setw(name_column_width) << "store " << i.target;
    if (!i.debug_info.name.empty()) {
        out << "    ; " << i.debug_info.name;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const Push& i)
{
    return out << std::left << std::setw(name_column_width) << "push " << i.value;
}

std::ostream& operator<<(std::ostream& out, const Pop&)
{
    return out << std::left << std::setw(name_column_width) << "pop";
}

std::ostream& operator<<(std::ostream& out, const Relative_Jump& i)
{
    return out << std::left << std::setw(name_column_width) << "jump " << (i.offset >= 0 ? "+" : "")
               << i.offset;
}

std::ostream& operator<<(std::ostream& out, const Relative_Jump_If& i)
{
    return out << std::left << std::setw(name_column_width) << "if " //
               << (i.expected ? "true" : "false") << " jump " //
               << (i.offset >= 0 ? "+" : "") << i.offset;
}

std::ostream& operator<<(std::ostream& out, const Break&)
{
    return out << "break";
}

std::ostream& operator<<(std::ostream& out, const Continue&)
{
    return out << "continue";
}

std::ostream& operator<<(std::ostream& out, const Return&)
{
    return out << "return";
}

std::ostream& operator<<(std::ostream& out, const Convert& i)
{
    return out << std::left << std::setw(name_column_width) << "convert to " << i.type;
}

std::ostream& operator<<(std::ostream& out, const Unary_Operate& i)
{
    return out << "unary " << expression_type_code_name(i.op);
}

std::ostream& operator<<(std::ostream& out, const Binary_Operate& i)
{
    return out << "binary " << expression_type_code_name(i.op);
}

std::ostream& operator<<(std::ostream& out, const Call& i)
{
    return out << std::left << std::setw(name_column_width) << "call " << i.address;
}

std::ostream& operator<<(std::ostream& out, const Builtin_Call& i)
{
    return out << std::left << std::setw(name_column_width) << "call builtin "
               << builtin_function_name(i.function);
}

} // namespace ins

std::ostream& operator<<(std::ostream& out, const Instruction& i)
{
    visit([&out](const auto& n) { out << n; }, i);
    return out;
}

void dump_program(std::ostream& out, std::span<const Instruction> instructions)
{
    for (const auto& i : instructions) {
        out << i << '\n';
    }
}

} // namespace bit_manipulation::bms
