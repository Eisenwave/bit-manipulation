#include <algorithm>
#include <ranges>
#include <vector>

#include <gtest/gtest.h>

#include "bmd/codegen/declarations.hpp"

namespace bit_manipulation {
namespace {

struct Declaration {
    Size index;
    bool is_forward;

    [[maybe_unused]] // suppresses https://github.com/llvm/llvm-project/issues/125233
    friend std::strong_ordering
    operator<=>(const Declaration&, const Declaration&)
        = default;
};

struct [[nodiscard]] Dependency_Break_Result {
    std::pmr::vector<Declaration> data;

    [[nodiscard]] bool empty() const
    {
        return data.empty();
    }

    [[nodiscard]] friend constexpr bool operator==(const Dependency_Break_Result& self,
                                                   std::span<const Declaration> other)
    {
        return std::ranges::equal(self.data, other);
    }

    [[nodiscard]] bool has_no_forward_dependencies(std::span<const bmd::Edge> edge) const
    {
        return std::ranges::all_of(edge, [&](const bmd::Edge e) -> bool {
            const auto caller_pos
                = std::ranges::find(data, Declaration { e.from, false }) - data.begin();
            const auto callee_pos
                = std::ranges::find(data, e.to, &Declaration::index) - data.begin();
            return callee_pos <= caller_pos;
        });
    }

    [[nodiscard]] bool defines_all(Size n) const
    {
        return std::ranges::all_of(std::views::iota(Size { 0 }, n), [&](const Size index) {
            return std::ranges::find(data, Declaration { index, false }) != data.end();
        });
    }

    [[nodiscard]] bool has_no_duplicates() const
    {
        for (Size i = 0; i < data.size(); ++i) {
            for (Size j = 0; j < data.size(); ++j) {
                if (i != j && data[i] == data[j]) {
                    return false;
                }
            }
        }
        return true;
    }
};

Dependency_Break_Result break_dependencies(Size n,
                                           std::span<const bmd::Edge> dependencies,
                                           std::pmr::memory_resource* memory)
{
    std::pmr::vector<Declaration> result { memory };
    result.reserve(n * 2);
    break_dependencies(
        [&](Size index, bool is_forward) { result.push_back({ index, is_forward }); }, n,
        dependencies, memory);

    return { result };
}

[[nodiscard]] std::pmr::vector<bmd::Edge>
generate_dependencies(Size n,
                      Function_Ref<bool(Size, Size)> filter,
                      std::pmr::memory_resource* memory)
{
    std::pmr::vector<bmd::Edge> result { memory };
    for (Size from = 0; from < n; ++from) {
        for (Size to = 0; to < n; ++to) {
            if (filter(from, to)) {
                result.push_back({ from, to });
            }
        }
    }
    return result;
}

[[maybe_unused]] [[nodiscard]] Dependency_Break_Result
break_generated_dependencies(Size n,
                             Function_Ref<bool(Size, Size)> filter,
                             std::pmr::memory_resource* memory)
{
    const std::pmr::vector<bmd::Edge> d = generate_dependencies(n, filter, memory);
    return break_dependencies(n, d, memory);
}

TEST(Dependency_Breaking, empty)
{
    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(0, {}, &memory);
    EXPECT_TRUE(actual.empty());
}

TEST(Dependency_Breaking, single)
{
    constexpr Declaration expected[] = { { 0, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(1, {}, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, no_reorder_for_no_dependencies)
{
    constexpr Declaration expected[]
        = { { 0, false }, { 1, false }, { 2, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, {}, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, no_reorder_for_backwards_simple)
{
    constexpr bmd::Edge input[] = { { 1, 0 }, { 2, 1 }, { 3, 2 }, { 4, 3 } };
    constexpr Declaration expected[]
        = { { 0, false }, { 1, false }, { 2, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, reverse_for_forwards_only)
{
    constexpr bmd::Edge input[] = { { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 4 } };
    constexpr Declaration expected[]
        = { { 4, false }, { 3, false }, { 2, false }, { 1, false }, { 0, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, last_in_cycle_forward_declared)
{
    constexpr bmd::Edge input[] = { { 1, 2 }, { 2, 3 }, { 3, 1 } };
    constexpr Declaration expected[]
        = { { 0, false }, { 3, true }, { 2, false }, { 1, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, test_exhaustively_4)
{
    constexpr Size n = 4;
    std::pmr::unsynchronized_pool_resource memory;

    std::pmr::vector<bmd::Edge> edges { &memory };
    edges.reserve(n * n);

    for (Uint16 edge_bits = 0;;) {
        edges.clear();
        for (Size from = 0; from < n; ++from) {
            for (Size to = 0; to < n; ++to) {
                const Size index = (from << 2) | to;
                if ((edge_bits >> index) & 1) {
                    edges.push_back({ from, to });
                }
            }
        }

        const auto result = break_dependencies(n, edges, &memory);
        const bool defines_all = result.defines_all(n);
        const bool no_dupes = result.has_no_duplicates();
        const bool no_forwards = result.has_no_forward_dependencies(edges);

        BIT_MANIPULATION_ASSERT(defines_all && no_dupes && no_forwards);

        if (Uint16(++edge_bits) == 0) {
            break;
        }
    }
}

} // namespace
} // namespace bit_manipulation
