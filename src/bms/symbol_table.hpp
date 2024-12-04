#ifndef BIT_MANIPULATION_SYMBOL_TABLE_HPP
#define BIT_MANIPULATION_SYMBOL_TABLE_HPP

#include <memory_resource>
#include <string_view>
#include <unordered_map>

#include "common/variant.hpp"

#include "bms/ast.hpp"
#include "bms/lookup_result.hpp"

namespace bit_manipulation::bms {

/// @brief A hierarchical associative container which maps non-owning keys (`std::string_view`) onto
/// AST nodes (`Lookup_Result`).
/// A symbol table can be created with a parent symbol table (see `From_Parent_Tag`),
/// in which case, if lookup fails in this table, lookup will also continue in the parent table.
/// This mirrors the lookup behavior in a hierarchy of scopes.
struct Symbol_Table {
public:
    struct From_Parent_Tag { };
    using key_type = std::string_view;
    using value_type = Lookup_Result;
    using map_type = std::pmr::unordered_map<key_type, value_type>;

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

    /// @brief Attempts to emplace a new symbol in the table.
    /// @param symbol the symbol name
    /// @param node the node to which the symbol refers
    /// @param shadow if `true`, permits placing a symbol, even if one in the parent table conflicts
    /// (duplicates at the same level are not possible though)
    /// @return an iterator to the emplaced pair upon success, or a lookup result to the existing
    /// node with the given name upon failure
    Variant<map_type::iterator, value_type> emplace(key_type symbol, value_type node, bool shadow)
    {
        if (!shadow && m_parent != nullptr) {
            if (Optional_Lookup_Result old = m_parent->find(symbol)) {
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

    Optional_Lookup_Result find(key_type symbol) const
    {
        if (m_parent != nullptr) {
            if (auto parent_result = m_parent->find(symbol)) {
                return parent_result;
            }
        }
        if (auto iter = m_symbols.find(symbol); iter != m_symbols.end()) {
            return iter->second;
        }
        return {};
    }
};

} // namespace bit_manipulation::bms

#endif
