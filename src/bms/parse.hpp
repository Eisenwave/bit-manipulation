#ifndef BIT_MANIPULATION_BMS_PARSING_HPP
#define BIT_MANIPULATION_BMS_PARSING_HPP

#include <span>
#include <string_view>
#include <vector>

#include "common/config.hpp"
#include "common/result.hpp"

#include "bms/astp.hpp"
#include "bms/diagnostic_consumer.hpp"
#include "bms/fwd.hpp"
#include "bms/parse_error.hpp"

namespace bit_manipulation::bms {

struct Parsed_Program {
private:
    std::pmr::vector<astp::Some_Node> m_nodes;
    std::string_view m_source;
    astp::Handle m_root_node = astp::Handle::null;

public:
    [[nodiscard]] explicit Parsed_Program(std::string_view source,
                                          std::pmr::memory_resource* memory)
        : m_nodes(memory)
        , m_source(source)
    {
    }

    [[nodiscard]] Parsed_Program() = default;

    Parsed_Program(const Parsed_Program&) = delete;
    Parsed_Program& operator=(const Parsed_Program&) = delete;

    [[nodiscard]] Parsed_Program(Parsed_Program&&) = default;
    Parsed_Program& operator=(Parsed_Program&&) = default;

    /// @brief Return the program source code.
    [[nodiscard]] std::string_view get_source() const
    {
        return m_source;
    }

    /// @brief Returns a handle to the root node.
    [[nodiscard]] astp::Handle get_root_handle() const
    {
        return m_root_node;
    }

    void set_root_handle(astp::Handle root)
    {
        BIT_MANIPULATION_ASSERT(root != astp::Handle::null);
        m_root_node = root;
    }

    /// @brief Returns the number of parsed AST nodes.
    [[nodiscard]] Size get_node_count() const
    {
        return m_nodes.size();
    }

    astp::Some_Node& get_node(astp::Handle handle) &
    {
        BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
        BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < m_nodes.size());
        return m_nodes[static_cast<Size>(handle)];
    }

    const astp::Some_Node& get_node(astp::Handle handle) const&
    {
        BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
        BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < m_nodes.size());
        return m_nodes[static_cast<Size>(handle)];
    }

    astp::Handle push_node(astp::Some_Node&& node) &
    {
        const auto result = static_cast<astp::Handle>(m_nodes.size());
        m_nodes.push_back(std::move(node));
        return result;
    }

    [[nodiscard]] std::string_view extract(const Local_Source_Span& span) const
    {
        return m_source.substr(span.begin, span.length);
    }
};

Result<Parsed_Program, Parse_Error>
parse(std::span<const Token> tokens, std::string_view source, std::pmr::memory_resource* memory);

inline std::optional<Parsed_Program> parse(std::span<const Token> tokens,
                                           std::string_view source,
                                           std::pmr::memory_resource* memory,
                                           Diagnostic_Consumer& diagnostics)
{
    if (auto result = parse(tokens, source, memory)) {
        return std::move(result.value());
    }
    else {
        diagnostics(std::move(result.error()));
        return {};
    }
}

} // namespace bit_manipulation::bms

#endif
