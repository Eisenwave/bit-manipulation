#ifndef BIT_MANIPULATION_BMD_TOKENS_HPP
#define BIT_MANIPULATION_BMD_TOKENS_HPP

#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "common/config.hpp"
#include "common/result.hpp"
#include "common/source_position.hpp"
#include "common/variant.hpp"

#include "bmd/fwd.hpp"
#include "bmd/parsing/ast.hpp"

namespace bit_manipulation::bmd {

struct [[nodiscard]] Parsed_Document {
    std::string_view source;
    std::pmr::vector<ast::Content> content;
};

/// @brief Parses the BMD document.
///
/// Note that parsing is infallible.
/// In the BMD grammar, any syntax violation can fall back onto literal text,
/// so the parsed result may be undesirable, but always valid.
/// @param source the BMD source code
/// @param memory memory for storing vectors of parsed elements etc.
/// @return the parsed document
Parsed_Document parse(std::string_view source, std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bmd

#endif
