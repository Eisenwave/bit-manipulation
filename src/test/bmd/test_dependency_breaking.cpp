#include <algorithm>
#include <memory_resource>
#include <random>
#include <ranges>
#include <vector>

#include <gtest/gtest.h>

#include "bmd/codegen/declarations.hpp"

namespace bit_manipulation {
namespace {

struct [[nodiscard]] Dependency_Break_Result {
    std::pmr::vector<bmd::Declaration> data;

    [[nodiscard]] bool empty() const
    {
        return data.empty();
    }

    [[nodiscard]] friend constexpr bool operator==(const Dependency_Break_Result& self,
                                                   std::span<const bmd::Declaration> other)
    {
        return std::ranges::equal(self.data, other);
    }

    [[nodiscard]] bool has_no_forward_dependencies(std::span<const bmd::Edge> edge) const
    {
        return std::ranges::all_of(edge, [&](const bmd::Edge e) -> bool {
            const auto caller_pos
                = std::ranges::find(data, bmd::Declaration { e.from, false }) - data.begin();
            const auto callee_pos
                = std::ranges::find(data, e.to, &bmd::Declaration::index) - data.begin();
            return callee_pos <= caller_pos;
        });
    }

    [[nodiscard]] bool defines_all(Size n) const
    {
        return std::ranges::all_of(std::views::iota(Size { 0 }, n), [&](const Size index) {
            return std::ranges::find(data, bmd::Declaration { index, false }) != data.end();
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
    Dependency_Break_Result result { std::pmr::vector<bmd::Declaration> { memory } };
    break_dependencies(result.data, n, dependencies, memory);
    return result;
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
    constexpr bmd::Declaration expected[] = { { 0, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(1, {}, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, no_reorder_for_no_dependencies)
{
    constexpr bmd::Declaration expected[]
        = { { 0, false }, { 1, false }, { 2, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, {}, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, no_reorder_for_backwards_simple)
{
    constexpr bmd::Edge input[] = { { 1, 0 }, { 2, 1 }, { 3, 2 }, { 4, 3 } };
    constexpr bmd::Declaration expected[]
        = { { 0, false }, { 1, false }, { 2, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, reverse_for_forwards_only)
{
    constexpr bmd::Edge input[] = { { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 4 } };
    constexpr bmd::Declaration expected[]
        = { { 4, false }, { 3, false }, { 2, false }, { 1, false }, { 0, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, last_in_cycle_forward_declared)
{
    constexpr bmd::Edge input[] = { { 1, 2 }, { 2, 3 }, { 3, 1 } };
    constexpr bmd::Declaration expected[]
        = { { 0, false }, { 3, true }, { 2, false }, { 1, false }, { 3, false }, { 4, false } };

    std::pmr::monotonic_buffer_resource memory;
    auto actual = break_dependencies(5, input, &memory);

    EXPECT_EQ(actual, expected);
}

TEST(Dependency_Breaking, test_exhaustively_4)
{
    constexpr Size n = 4;
    constexpr Size log2_n = 2;
    std::pmr::unsynchronized_pool_resource memory;

    std::pmr::vector<bmd::Edge> edges { &memory };
    edges.reserve(n * n);

    for (Uint16 edge_bits = 0;;) {
        edges.clear();
        for (Size from = 0; from < n; ++from) {
            for (Size to = 0; to < n; ++to) {
                const Size index = (from << log2_n) | to;
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

TEST(Dependency_Breaking, test_randomly_8)
{
    constexpr Size n = 8;
    constexpr Size log2_n = 3;
    constexpr Size samples = 10'000;

    std::pmr::unsynchronized_pool_resource memory;

    std::default_random_engine rng { 12345 };
    std::uniform_int_distribution<Uint64> distr;

    std::pmr::vector<bmd::Edge> edges { &memory };
    edges.reserve(n * n);

    for (Size i = 0; i < samples; ++i) {
        const Uint64 edge_bits = distr(rng);

        edges.clear();
        for (Size from = 0; from < n; ++from) {
            for (Size to = 0; to < n; ++to) {
                const Size index = (from << log2_n) | to;
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
    }
}

} // namespace
} // namespace bit_manipulation
