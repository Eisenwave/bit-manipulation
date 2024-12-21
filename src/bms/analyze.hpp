#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <memory_resource>

#include "common/function_ref.hpp"
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
/// Validates whether all functions that return something other than `Void` definitely return.
Result<void, Analysis_Error> analyze_returning(Analyzed_Program& program);

/// @brief Runs `analyze_returning` for just a single function instead of the whole program.
///
/// This is useful because while return analysis is considered a separate stage,
/// it is run early for the sake of convenience so that VM codegen can use this information
/// to determine whether `Void` functions already return or require an implicit `return`.
/// @return `true` if the function definitely returns;
/// note that functions that are missing a return statement will still produce an error;
/// the returned value is essentially only useful for analyzing `Void` functions,
/// where omitting `return` is permitted
Result<bool, Analysis_Error> analyze_returning(Analyzed_Program& program,
                                               const ast::Some_Node* function_node,
                                               const ast::Function& f);

/// @brief The fifth stage of analysis.
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
Result<void, Analysis_Error> analyze(Analyzed_Program& program,
                                     const Parsed_Program& parsed,
                                     std::pmr::memory_resource* memory_resource);

bool analyze(Analyzed_Program& program,
             const Parsed_Program& parsed,
             std::pmr::memory_resource* memory_resource,
             Function_Ref<Error_Reaction(Analysis_Error&&)> on_error);

} // namespace bit_manipulation::bms

#endif
