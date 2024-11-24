#ifndef BIT_MANIPULATION_SYMBOL_TABLE_HPP
#define BIT_MANIPULATION_SYMBOL_TABLE_HPP

#include <memory_resource>
#include <string_view>
#include <unordered_map>
#include <variant>

#include "bms/ast.hpp"

namespace bit_manipulation::bms {

/// @brief A hierarchical associative container which maps non-owning keys (`std::string_view`) onto
/// AST nodes (`ast::Some_Node*`).
/// A symbol table can be created with a parent symbol table (see `From_Parent_Tag`),
/// in which case, if lookup fails in this table, lookup will also continue in the parent table.
/// This mirrors the lookup behavior in a hierarchy of scopes.
struct Symbol_Table {
public:
    struct From_Parent_Tag { };
    using map_type = std::pmr::unordered_map<std::string_view, ast::Some_Node*>;

private:
    map_type m_symbols;
    const Symbol_Table* m_parent = nullptr;

public:
    Symbol_Table(From_Parent_Tag, const Symbol_Table& parent, std::pmr::memory_resource* memory)
        : m_symbols(memory)
        , m_parent(&parent)
    {
    }

    Symbol_Table(From_Parent_Tag, const Symbol_Table& parent)
        : Symbol_Table(From_Parent_Tag {}, parent, parent.m_symbols.get_allocator().resource())
    {
    }

    explicit Symbol_Table(std::pmr::memory_resource* memory)
        : m_symbols(memory)
    {
    }

    explicit Symbol_Table(std::initializer_list<map_type::value_type> init,
                          std::pmr::memory_resource* memory = std::pmr::get_default_resource())
        : m_symbols(init, memory)
    {
    }

    Symbol_Table(const Symbol_Table&) = delete;
    Symbol_Table(Symbol_Table&&) = default;

    Symbol_Table& operator=(const Symbol_Table&) = delete;
    Symbol_Table& operator=(Symbol_Table&&) = default;

    std::variant<map_type::iterator, ast::Some_Node*> emplace(std::string_view symbol,
                                                              ast::Some_Node* node)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        if (m_parent != nullptr) {
            if (std::optional<ast::Some_Node*> old = m_parent->find(symbol)) {
                return *old;
            }
        }
        auto [iter, success] = m_symbols.emplace(symbol, node);
        if (success) {
            return iter;
        }
        else {
            return iter->second;
        }
    }

    std::optional<ast::Some_Node*> find(std::string_view symbol) const
    {
        if (m_parent != nullptr) {
            if (auto parent_result = m_parent->find(symbol)) {
                return parent_result;
            }
        }
        if (auto iter = m_symbols.find(symbol); iter != m_symbols.end()) {
            return iter->second;
        }
        return std::nullopt;
    }
};

} // namespace bit_manipulation::bms

#endif
