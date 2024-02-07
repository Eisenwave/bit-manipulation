#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <span>

#include "bms/analysis_error.hpp"
#include "bms/ast.hpp"
#include "bms/deduction.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

struct Analyzer_Base {
    Parsed_Program& m_program;
    ast::Program& m_root;

    Analyzer_Base(Parsed_Program& program);

    ast::Some_Node& get_node(ast::Node_Handle handle);
};

Result<void, Analysis_Error> analyze_name_lookup(Parsed_Program& program);
Result<void, Analysis_Error> analyze_semantics(Parsed_Program& program);

inline Result<void, Analysis_Error> analyze(Parsed_Program& program)
{
    if (auto r = analyze_name_lookup(program); !r) {
        return r;
    }
    return analyze_semantics(program);
}

} // namespace bit_manipulation::bms

#endif