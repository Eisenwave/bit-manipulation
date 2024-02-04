#ifndef BIT_MANIPULATION_BMS_TYPE_HPP
#define BIT_MANIPULATION_BMS_TYPE_HPP

#include <limits>
#include <variant>

#include "assert.hpp"
#include "config.hpp"

#include "bms/concrete_type.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

struct Bit_Generic_Type {
    Type_Type type;
    ast::Node_Handle width;

    constexpr Bit_Generic_Type(Type_Type type, ast::Node_Handle width)
        : type(type)
        , width(width)
    {
        BIT_MANIPULATION_ASSERT(width != ast::Node_Handle::null);
    }
};

using Some_Type = std::variant<Concrete_Type, Bit_Generic_Type>;

} // namespace bit_manipulation::bms

#endif