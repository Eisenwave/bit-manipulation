#ifndef BIT_MANIPULATION_LINEAR_MAP_STACK_HPP
#define BIT_MANIPULATION_LINEAR_MAP_STACK_HPP

#include <vector>

#include "result.hpp"

#include "bms/fwd.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

/// @brief A simple data structure which approximates std::vector<std::unordered_map>.
/// This data structure is intended to be used only for small amounts of data and relies on
/// linear search.
/// It stores key/value pairs and performs linear search based on the key to find corresponding
/// entries.
/// Furthermore, each "map" on the stack is delimited by a sentinel value, allowing you to
/// push and pop entries.
///
/// This structure is very useful for two reasons:
/// 1. It is cache-friendly and fast for the simple case of looking up variable values by their
///    keys. There are few variables, so linear search is sufficient and perhaps optimal.
/// 2. Return addresses can be associated with the sentinel keys, so that this data structure also
///    acts as a call stack.
struct Linear_Map_Stack {
    static constexpr ast::Node_Handle sentinel_key = ast::Node_Handle::null;
    struct Entry {
        ast::Node_Handle key;
        Concrete_Value value;
    };

private:
    std::vector<Entry> m_data;

public:
    Entry* find(ast::Node_Handle key)
    {
        BIT_MANIPULATION_ASSERT(key != sentinel_key);
        for (Size i = m_data.size(); i-- != 0 && m_data[i].key != sentinel_key;) {
            if (key == m_data[i].key) {
                return &m_data[i];
            }
        }
        return nullptr;
    }

    Result<Entry*, Entry*> emplace(ast::Node_Handle key, Concrete_Value value)
    {
        if (Entry* const existing = find(key)) {
            return { Error_Tag {}, existing };
        }
        return { Success_Tag {}, &m_data.emplace_back(key, value) };
    }

    Entry& assign(ast::Node_Handle key, Concrete_Value value)
    {
        if (Entry* const existing = find(key)) {
            existing->value = value;
            return *existing;
        }
        return m_data.emplace_back(key, value);
    }

    Entry& push_frame(Concrete_Value sentinel_value)
    {
        return m_data.emplace_back(sentinel_key, sentinel_value);
    }

    std::optional<Concrete_Value> pop_frame()
    {
        for (Size i = m_data.size(); i-- != 0;) {
            if (m_data[i].key == sentinel_key) {
                Concrete_Value result = m_data[i].value;
                m_data.resize(i);
                return result;
            }
        }
        return std::nullopt;
    }
};

} // namespace bit_manipulation::bms

#endif