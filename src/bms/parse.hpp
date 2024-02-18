#ifndef BIT_MANIPULATION_BMS_PARSING_HPP
#define BIT_MANIPULATION_BMS_PARSING_HPP

#include <span>
#include <string_view>
#include <vector>

#include "config.hpp"
#include "result.hpp"

#include "bms/astp.hpp"
#include "bms/fwd.hpp"
#include "bms/grammar.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

struct Parsed_Program {
    std::pmr::vector<astp::Some_Node> nodes;
    std::string_view source;
    astp::Handle root_node = astp::Handle::null;

    [[nodiscard]] explicit Parsed_Program(std::string_view source,
                                          std::pmr::memory_resource* memory)
        : nodes(memory)
        , source(source)
    {
    }

    [[nodiscard]] Parsed_Program() = default;

    Parsed_Program(const Parsed_Program&) = delete;
    Parsed_Program& operator=(const Parsed_Program&) = delete;

    [[nodiscard]] Parsed_Program(Parsed_Program&&) = default;
    Parsed_Program& operator=(Parsed_Program&&) = default;

    astp::Some_Node& get_node(astp::Handle handle) &
    {
        BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
        BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < nodes.size());
        return nodes[static_cast<Size>(handle)];
    }

    const astp::Some_Node& get_node(astp::Handle handle) const&
    {
        BIT_MANIPULATION_ASSERT(handle != astp::Handle::null);
        BIT_MANIPULATION_ASSERT(static_cast<Size>(handle) < nodes.size());
        return nodes[static_cast<Size>(handle)];
    }

    astp::Handle push_node(astp::Some_Node&& node) &
    {
        const auto result = static_cast<astp::Handle>(nodes.size());
        nodes.push_back(std::move(node));
        return result;
    }

    [[nodiscard]] std::string_view extract(const Local_Source_Span& span) const
    {
        return source.substr(span.begin, span.length);
    }
};

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

Result<Parsed_Program, Parse_Error>
parse(std::span<const Token> tokens, std::string_view source, std::pmr::memory_resource* memory);

} // namespace bit_manipulation::bms

#endif