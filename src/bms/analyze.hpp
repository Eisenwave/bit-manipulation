#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <memory_resource>

#include "common/result.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief The first stage of analysis.
/// After this stage completes, each AST node which holds annotations will have actual
/// `Annotation` objects stored, with parameters resolved,
/// rather than just referencing the parser's nodes.
Result<void, Analysis_Error> resolve_annotations(Analyzed_Program& program,
                                                 const Parsed_Program& parsed);

/// @brief The second stage of analysis.
/// Declarations are analyzed for name conflicts, id expressions and other AST nodes receive a
/// `lookup_result`, etc.
Result<void, Analysis_Error> analyze_name_lookup(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource);

/// @brief The third stage of analysis (and by far the most complex).
/// The semantic validity of language constructs,
/// adherence to the type system, etc. are verified.
/// Any constant expressions are resolved through constant evaluation,
/// and functions receive VM addresses.
Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program,
                                               std::pmr::memory_resource* memory_resource);

/// @brief The fourth stage of analysis.
/// Applies any language-agnostic annotations, such as `@inline`, `@unroll`, etc.
/// Also verifies that any annotation has been used properly by checking its arguments,
/// verifying that `@immutable` has only been applied to immutable variables, etc.
Result<void, Analysis_Error> process_annotations(Analyzed_Program& program,
                                                 std::pmr::memory_resource* memory_resource)
    = delete; // TODO: implement

/// @brief Runs all analyses on the given `Analyzed_Program`.
///
/// Analysis generally consists of multiple stages which are run in sequence:
/// `resolve_annotations`, `analyze_name_lookup`, `analyze_semantics`.
/// @param program the program to analyze
/// @param memory_resource memory resource for allocations during analysis
/// @return the analysis result
Result<void, Analysis_Error> analyze(Analyzed_Program& program,
                                     std::pmr::memory_resource* memory_resource);

bool analyze(Analyzed_Program& program,
             std::pmr::memory_resource* memory_resource,
             Diagnostic_Consumer& diagnostics);

} // namespace bit_manipulation::bms

#endif
