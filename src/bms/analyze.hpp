#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <span>

#include "bms/analysis_error.hpp"
#include "bms/ast.hpp"
#include "bms/fwd.hpp"

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

using Widths = std::variant<int, std::span<const int>>;

constexpr int get_width(const Widths& w, Size i)
{
    if (const auto* single_width = std::get_if<int>(&w)) {
        return *single_width;
    }
    else {
        auto& span = std::get<std::span<const int>>(w);
        BIT_MANIPULATION_ASSERT(i < span.size());
        return span[i];
    }
}

Result<void, Analysis_Error> analyze_name_lookup(Parsed_Program& program);

Result<void, Analysis_Error> analyze(Parsed_Program& program);

} // namespace bit_manipulation::bms

#endif