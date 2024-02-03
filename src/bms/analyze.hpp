#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include "bms/bms.hpp"

namespace bit_manipulation::bms {

struct Analyzer_Base {
    Parsed_Program& m_program;
    ast::Program_Node& m_root;

    Analyzer_Base(Parsed_Program& program);

    ast::Some_Node& get_node(ast::Node_Handle handle);
};

inline ast::Node_Handle get_bit_generic_expression(Some_Type& type)
{
    if (Bit_Generic_Type* g = std::get_if<Bit_Generic_Type>(&type)) {
        return g->width;
    }
    return ast::Node_Handle::null;
}

Result<void, Analysis_Error> analyze_name_lookup(Parsed_Program& program);

} // namespace bit_manipulation::bms

#endif