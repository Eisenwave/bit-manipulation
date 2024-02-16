#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <concepts>
#include <memory_resource>
#include <span>
#include <vector>

#include "bms/analysis_error.hpp"
#include "bms/ast.hpp"
#include "bms/deduction.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

struct Analyzed_Program {
private:
    std::string_view m_source;
    ast::Some_Node* m_root = nullptr;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::vector<ast::Some_Node*> m_nodes;

public:
    explicit Analyzed_Program(const Parsed_Program& program);

    Analyzed_Program(const Analyzed_Program&) = delete;
    Analyzed_Program& operator=(const Analyzed_Program&) = delete;

    ~Analyzed_Program()
    {
        for (ast::Some_Node* const& node : m_nodes) {
            std::destroy_at(node);
        }
    }

    [[nodiscard]] std::string_view get_source() const noexcept
    {
        return m_source;
    }

    [[nodiscard]] ast::Some_Node* get_root() const noexcept
    {
        return m_root;
    }

    [[nodiscard]] std::pmr::memory_resource* get_memory_resource() noexcept
    {
        return &m_memory_resource;
    }

    template <std::derived_from<ast::detail::Node_Base> T, typename... Args>
    [[nodiscard]] ast::Some_Node* emplace(Args&&... args)
    {
        void* storage = m_memory_resource.allocate(sizeof(ast::Some_Node), alignof(ast::Some_Node));
        try {
            auto* result = new (storage) ast::Some_Node(T(std::forward<Args>(args)...));
            m_nodes.push_back(result);
            return result;
        } catch (...) {
            m_memory_resource.deallocate(storage, sizeof(ast::Some_Node), alignof(ast::Some_Node));
            throw;
        }
    }

private:
    [[nodiscard]] ast::Some_Node* from_parser_node(astp::Handle, const Parsed_Program&);
};

struct Analyzer_Base {
    Analyzed_Program& m_program;

    Analyzer_Base(Analyzed_Program& program)
        : m_program { program }
    {
    }
};

Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program);
Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program);

inline Result<void, Analysis_Error> analyze(Analyzed_Program& program)
{
    if (auto r = analyze_name_lookup(program); !r) {
        return r;
    }
    return analyze_semantics(program);
}

} // namespace bit_manipulation::bms

#endif