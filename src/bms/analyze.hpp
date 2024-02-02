#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include "bms/bms.hpp"

namespace bit_manipulation::bms {

struct Analyzer_Base {
    Parsed_Program& m_program;
    ast::Program_Node& m_root;

    Analyzer_Base(Parsed_Program& program)
        : m_program(program)
        , m_root(std::get<ast::Program_Node>(program.get_node(program.root_node)))
    {
    }

    ast::Some_Node& get_node(ast::Node_Handle handle)
    {
        return m_program.get_node(handle);
    }
};

inline ast::Node_Handle get_bit_generic_expression(Some_Type& type)
{
    if (Bit_Generic_Type* g = std::get_if<Bit_Generic_Type>(&type)) {
        return g->width;
    }
    return Node_Handle::null;
}

Analysis_Result analyze_name_lookup(Parsed_Program& program);

} // namespace bit_manipulation::bms

#endif