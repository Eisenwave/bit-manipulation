#ifndef BIT_MANIPULATION_BMS_ANALYZED_PROGRAM_HPP
#define BIT_MANIPULATION_BMS_ANALYZED_PROGRAM_HPP

#include <memory_resource>

#include "common/result.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Introspection_Error_Code {
    /// @brief No entity was found during lookup.
    nothing_found,
    /// @brief The wrong kind of entity was found (e.g. a function when looking for a constant).
    wrong_entity
};

/// @brief A class which represents an analyzed program this is under analysis
/// or which has already been analyzed.
///
/// Program analysis is currently split into two stages; name lookup analysis and everything else.
/// "Everything else" includes type-checking, instantiations based on bit-generic types,
/// constant evaluation, etc.
struct Analyzed_Program {
private:
    struct Implementation;
    std::pmr::memory_resource* m_memory = nullptr;
    Implementation* m_impl = nullptr;

public:
    /// @brief Constructs an analyzed program from a parsed program.
    /// This has no effect on the parsed program, which has a slightly different AST representation
    /// and is translated into the "analysis AST" immediately upon construction.
    /// @param program the parsed program
    /// @param file_name the file name, used for diagnostics
    /// @param memory the memory resource to be used during analyses
    explicit Analyzed_Program(const Parsed_Program& program,
                              std::string_view file_name,
                              std::pmr::memory_resource* memory);

    Analyzed_Program(const Analyzed_Program&) = delete;
    Analyzed_Program& operator=(const Analyzed_Program&) = delete;

    Analyzed_Program(Analyzed_Program&& other) noexcept;

    Analyzed_Program& operator=(Analyzed_Program&& other) noexcept;

    ~Analyzed_Program();

    [[nodiscard]] Virtual_Machine& get_vm();

    [[nodiscard]] std::string_view get_source() const;

    [[nodiscard]] std::string_view get_file_name() const;

    [[nodiscard]] ast::Some_Node* get_root() const;

    [[nodiscard]] std::pmr::memory_resource* get_memory_resource() const;

    [[nodiscard]] ast::Some_Node* insert(const ast::Some_Node& node);

    [[nodiscard]] ast::Some_Node* insert(ast::Some_Node&&);

    [[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
    find_entity(std::string_view name) const;

    [[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
    find_global_function_node(std::string_view name) const;

    [[nodiscard]] Result<const ast::Some_Node*, Introspection_Error_Code>
    find_global_constant_node(std::string_view name) const;

    [[nodiscard]] Result<const ast::Function*, Introspection_Error_Code>
    find_global_function(std::string_view name) const;

    [[nodiscard]] Result<const ast::Const*, Introspection_Error_Code>
    find_global_constant(std::string_view name) const;

private:
    std::pmr::polymorphic_allocator<> allocator() const;
};

} // namespace bit_manipulation::bms

#endif
