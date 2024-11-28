#ifndef BIT_MANIPULATION_BMS_ANALYZE_HPP
#define BIT_MANIPULATION_BMS_ANALYZE_HPP

#include <memory_resource>

#include "common/result.hpp"

#include "bms/analysis_error.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

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

bool analyze(Analyzed_Program& program,
             std::pmr::memory_resource* memory_resource,
             Diagnostic_Consumer& diagnostics);

} // namespace bit_manipulation::bms

#endif
