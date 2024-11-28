#ifndef BIT_MANIPULATION_BMS_PARSING_HPP
#define BIT_MANIPULATION_BMS_PARSING_HPP

#include <optional>
#include <span>
#include <string_view>

#include "common/config.hpp"
#include "common/result.hpp"

#include "bms/fwd.hpp"
#include "bms/parse_error.hpp"

namespace bit_manipulation::bms {

struct Parsed_Program {
private:
    struct Implementation;

    std::pmr::memory_resource* m_memory = nullptr;
    Implementation* m_impl;

public:
    [[nodiscard]] explicit Parsed_Program(std::string_view source,
                                          std::pmr::memory_resource* memory);

    Parsed_Program(const Parsed_Program&) = delete;
    Parsed_Program& operator=(const Parsed_Program&) = delete;

    [[nodiscard]] Parsed_Program(Parsed_Program&& other) noexcept;

    Parsed_Program& operator=(Parsed_Program&&) noexcept;

    ~Parsed_Program();

    /// @brief Return the program source code.
    [[nodiscard]] std::string_view get_source() const;

    /// @brief Returns a handle to the root node.
    [[nodiscard]] astp::Handle get_root_handle() const;

    void set_root_handle(astp::Handle root);

    /// @brief Returns the number of parsed AST nodes.
    [[nodiscard]] Size get_node_count() const;

    astp::Some_Node& get_node(astp::Handle handle) &;

    const astp::Some_Node& get_node(astp::Handle handle) const&;

    [[nodiscard]] std::string_view extract(const Local_Source_Span& span) const;

    void downsize_nodes(Size n) &;

    astp::Handle push_node(astp::Some_Node&& node) &;

    std::pmr::memory_resource* get_memory() const
    {
        return m_memory;
    }

private:
    std::pmr::polymorphic_allocator<> allocator() const;
};

Result<void, Parse_Error> parse(Parsed_Program& program, std::span<const Token> tokens);

bool parse(Parsed_Program& program,
           std::span<const Token> tokens,
           Diagnostic_Consumer& diagnostics);

} // namespace bit_manipulation::bms

#endif
