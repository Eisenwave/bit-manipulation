#ifndef BIT_MANIPULATION_BMS_PARSING_HPP
#define BIT_MANIPULATION_BMS_PARSING_HPP

#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "assert.hpp"
#include "config.hpp"
#include "result.hpp"

#include "bms/ast.hpp"
#include "bms/fwd.hpp"
#include "bms/grammar.hpp"
#include "bms/tokens.hpp"

namespace bit_manipulation::bms {

struct Parsed_Program {
    std::vector<ast::Some_Node> nodes;
    std::string_view source;
    ast::Handle root_node;

    ast::Some_Node& get_node(ast::Handle handle) &
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Handle::null);
        return nodes[static_cast<Size>(handle)];
    }

    const ast::Some_Node& get_node(ast::Handle handle) const&
    {
        BIT_MANIPULATION_ASSERT(handle != ast::Handle::null);
        return nodes[static_cast<Size>(handle)];
    }

    ast::Handle push_node(ast::Some_Node&& node) &
    {
        const auto result = static_cast<ast::Handle>(nodes.size());
        nodes.push_back(std::move(node));
        return result;
    }
};

struct Parse_Error {
    Grammar_Rule fail_rule;
    std::span<const Token_Type> expected_tokens;
    Token fail_token;
};

Result<Parsed_Program, Parse_Error> parse(std::span<const Token> tokens, std::string_view source);

} // namespace bit_manipulation::bms

#endif