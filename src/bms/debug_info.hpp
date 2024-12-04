#ifndef BIT_MANIPULATION_BMS_DEBUG_INFO_HPP
#define BIT_MANIPULATION_BMS_DEBUG_INFO_HPP

#include <optional>

#include "common/source_position.hpp"

namespace bit_manipulation::bms {

enum struct Construct : Default_Underlying {
    program,
    function,
    parameter,
    type,
    constant,
    variable,
    static_assertion,
    if_statement,
    while_statement,
    break_statement,
    continue_statement,
    return_statement,
    assignment,
    block_statement,
    conversion_expression,
    if_expression,
    binary_expression,
    prefix_expression,
    function_call_expression,
    id_expression,
    literal,
    builtin_function,
    annotation
};

struct Debug_Info {
    Construct construct;
    std::optional<Source_Position> pos;

    constexpr Debug_Info() noexcept
        : construct {}
        , pos {}
    {
    }

    constexpr Debug_Info(Construct construct, std::optional<Source_Position> pos)
        : construct(construct)
        , pos(pos)
    {
    }

    Debug_Info(const ast::Some_Node*);
};

} // namespace bit_manipulation::bms

#endif
