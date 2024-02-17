#include <ranges>

#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/parse.hpp"

namespace bit_manipulation::bms {

namespace ast {

Function::Function(const Function& other, Copy_for_Instantiation_Tag)
    : detail::Node_Base(other)
    , detail::Parent<4>(other)
    , m_name(other.m_name)
{
    set_children(other.m_children);
}

} // namespace ast

ast::Some_Node* Analyzed_Program::from_parser_node(astp::Handle handle,
                                                   const Parsed_Program& parsed)
{
    if (handle == astp::Handle::null) {
        return nullptr;
    }
    const auto transform_child
        = [this, &parsed](astp::Handle h) { return from_parser_node(h, parsed); };

    const auto child_from_parsed = [this]<typename T>(const T& n) {
        using Result = typename T::AST_Node;
        if constexpr (requires { Result(n, &m_memory_resource); }) {
            return emplace<Result>(n, &m_memory_resource);
        }
        else {
            return emplace<Result>(n);
        }
    };

    return fast_visit(
        [this, transform_child, child_from_parsed]<typename T>(const T& n) {
            using Result = typename T::AST_Node;
            ast::Some_Node* result = child_from_parsed(n);
            std::get<Result>(*result).set_children(n.get_children()
                                                   | std::views::transform(transform_child));
            return result;
        },
        parsed.get_node(handle));
}

Analyzed_Program::Analyzed_Program(const Parsed_Program& program, std::pmr::memory_resource* memory)
    : m_source(program.source)
    , m_memory_resource(memory)
    , m_nodes(memory)
{
    m_nodes.reserve(program.nodes.size());
    m_root = from_parser_node(program.root_node, program);
    BIT_MANIPULATION_ASSERT(m_root != nullptr);
}

} // namespace bit_manipulation::bms
