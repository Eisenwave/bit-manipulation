#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <concepts>
#include <memory_resource>
#include <span>
#include <vector>

#include "bms/analysis_error.hpp"
#include "bms/deduction.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

struct Analyzed_Program {
private:
    std::string_view m_source;
    std::string_view m_file_name;
    ast::Some_Node* m_root = nullptr;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::vector<ast::Some_Node*> m_nodes;

public:
    explicit Analyzed_Program(const Parsed_Program& program,
                              std::string_view file_name,
                              std::pmr::memory_resource* memory);

    Analyzed_Program(const Analyzed_Program&) = delete;
    Analyzed_Program& operator=(const Analyzed_Program&) = delete;

    ~Analyzed_Program()
    {
        for (ast::Some_Node* const& node : m_nodes) {
            std::destroy_at(node);
        }
    }

    [[nodiscard]] std::string_view get_source() const
    {
        return m_source;
    }

    [[nodiscard]] std::string_view get_file_name() const
    {
        return m_file_name;
    }

    [[nodiscard]] ast::Some_Node* get_root() const
    {
        return m_root;
    }

    [[nodiscard]] std::pmr::memory_resource* get_memory_resource()
    {
        return &m_memory_resource;
    }

    [[nodiscard]] ast::Some_Node* insert(const ast::Some_Node&);
    [[nodiscard]] ast::Some_Node* insert(ast::Some_Node&&);

private:
    template <typename T, typename... Args>
    [[nodiscard]] ast::Some_Node* emplace(Args&&... args);

private:
    [[nodiscard]] ast::Some_Node* from_parser_node(astp::Handle, const Parsed_Program&);
};

Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource);
Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program,
                                               std::pmr::memory_resource* memory_resource);

Result<void, Analysis_Error> analyze(Analyzed_Program& program,
                                     std::pmr::memory_resource* memory_resource);

} // namespace bit_manipulation::bms

#endif