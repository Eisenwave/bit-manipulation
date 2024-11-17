#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <concepts>
#include <memory_resource>
#include <span>

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief A class which represents an analyzed program this is under analysis
/// or which has already been analyzed.
///
/// Program analysis is currently split into two stages; name lookup analysis and everything else.
/// "Everything else" includes type-checking, instantiations based on bit-generic types,
/// constant evaluation, etc.
struct Analyzed_Program {
private:
    struct Implementation;
    Implementation* m_impl;

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
    ~Analyzed_Program();

    [[nodiscard]] std::string_view get_source() const;

    [[nodiscard]] std::string_view get_file_name() const;

    [[nodiscard]] ast::Some_Node* get_root() const;

    [[nodiscard]] std::pmr::memory_resource* get_memory_resource() const;

    [[nodiscard]] ast::Some_Node* insert(const ast::Some_Node& node);

    [[nodiscard]] ast::Some_Node* insert(ast::Some_Node&&);
};

Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource);
Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program,
                                               std::pmr::memory_resource* memory_resource);

/// @brief Runs all analyses on the given `Analyzed_Program`
/// @param program the program to analyze
/// @param memory_resource memory resource for allocations during analysis
/// @return the analysis result
Result<void, Analysis_Error> analyze(Analyzed_Program& program,
                                     std::pmr::memory_resource* memory_resource);

} // namespace bit_manipulation::bms

#endif
