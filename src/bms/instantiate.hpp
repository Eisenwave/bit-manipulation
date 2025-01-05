#ifndef BIT_MANIPULATION_BMS_INSTANTIATE_HPP
#define BIT_MANIPULATION_BMS_INSTANTIATE_HPP

#include <memory_resource>

#include "common/result.hpp"

#include "bms/ast.hpp"
#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

/// @brief Instantiates every bit-generic function in the program using the same `Widths`.
/// This is mostly useful for testing purposes and cannot be triggered "naturally" through
/// implicit instantiations.
[[nodiscard]] Result<void, Analysis_Error>
instantiate_all(Analyzed_Program& program, std::pmr::memory_resource* memory, const Widths& w);

/// @brief Instantiates an individual function.
/// Contrary to `instantiate_all`, this may be necessitated by implicit instantiations,
/// for example when a function taking `Uint(N)` is called.
///
/// At a high level, the process of instantiation works as follows:
///   1. Make a `deep_copy` of the node to be instantiated
///   2. `substitute_widths` to replace occurrences of bit-generic parameters with constants
///   3. Append the resulting `ast::Function::Instance` to the existing node
/// @param h the pointer to the function node
/// @param node the function node
/// @param w the widths for instantiation
/// @return the instantiated instance, or `Analysis_Error`
[[nodiscard]] Result<ast::Function::Instance*, Analysis_Error>
instantiate_function(Analyzed_Program& program,
                     std::pmr::memory_resource* memory,
                     ast::Some_Node* h,
                     ast::Function& node,
                     const Widths& w);

} // namespace bit_manipulation::bms

#endif
