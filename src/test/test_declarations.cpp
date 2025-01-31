#include <memory_resource>
#include <ranges>
#include <unordered_map>
#include <unordered_set>

#include <gtest/gtest.h>

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"

#include "bmd/codegen/declarations.hpp"

#include "test/program_file_testing.hpp"

namespace bit_manipulation {
namespace {

struct Dependency_By_Name {
    std::string_view name;
    bmd::Dependency_Type type;

    [[maybe_unused]] // suppresses https://github.com/llvm/llvm-project/issues/125233
    friend std::strong_ordering
    operator<=>(const Dependency_By_Name&, const Dependency_By_Name&)
        = default;
};

} // namespace
} // namespace bit_manipulation

template <>
struct std::hash<bit_manipulation::Dependency_By_Name> {
    [[nodiscard]] std::size_t operator()(const bit_manipulation::Dependency_By_Name& d) const
    {
        return std::hash<std::string_view> {}(d.name)
            ^ std::hash<bit_manipulation::Default_Underlying> {}(
                   bit_manipulation::Default_Underlying(d.type));
    }
};

namespace bit_manipulation {
namespace {

/// @brief Returns the name of a global declaration (function or constant),
/// or an empty string if the node has no name,
/// such a `static_assert` etc.
[[nodiscard]] std::string_view get_global_declaration_name(const bms::ast::Some_Node& node)
{
    if (const auto* const function = get_if<bms::ast::Function>(&node)) {
        return function->get_name();
    }
    if (const auto* const constant = get_if<bms::ast::Const>(&node)) {
        return constant->get_name();
    }
    return {};
}

using Dependency_Set = std::pmr::unordered_set<Dependency_By_Name>;
using Dependency_Map = std::pmr::unordered_map<std::string_view, Dependency_Set>;
using Dependency_Initializer_List = std::initializer_list<Dependency_Map::value_type>;

enum struct Dependency_Search_Type : Default_Underlying {
    /// @brief Search for direct dependencies on functions.
    direct_functions,
    /// @brief Search for recursive dependencies on functions.
    functions_recursively
};

bool dependency_search_type_is_recursive(Dependency_Search_Type search_type)
{
    return search_type == Dependency_Search_Type::functions_recursively;
}

[[nodiscard]] Dependency_Set gather_dependencies(const bms::ast::Some_Node& node,
                                                 std::pmr::memory_resource* memory,
                                                 Dependency_Search_Type search_type)
{
    Dependency_Set result { memory };

    bmd::for_each_global_dependency(
        [&,
         recursive = dependency_search_type_is_recursive(search_type)](bmd::Dependency d) -> bool {
            if (const auto* function = get_if<bms::ast::Function>(d.declaration)) {
                auto [_, success] = result.emplace(function->get_name(), d.type);
                return recursive && success;
            }
            return recursive;
        },
        node);

    return result;
}

[[nodiscard]] Dependency_Map gather_all_dependencies(const bms::Analyzed_Program& program,
                                                     std::pmr::memory_resource* memory,
                                                     Dependency_Search_Type search_type)
{
    Dependency_Map result { memory };

    const auto& root = get<bms::ast::Program>(*program.get_root());
    for (const bms::ast::Some_Node* child : root.get_children()) {
        const std::string_view key = get_global_declaration_name(*child);
        if (key.empty() || result.contains(key)) {
            continue;
        }
        result.emplace(key, gather_dependencies(*child, memory, search_type));
    }

    return result;
}

/// @brief Returns `true` if the dependencies in the program compiled from `file` equal the
/// `expected` dependencies,
/// where keys are the names of global declarations and values are sets of their dependencies.
///
/// Note that memory may be allocated while gathering dependencies from the program,
/// using `expected.get_allocator()`.
///
/// Also note that the test is simply for exact equality, so if there is a declaration with no
/// dependencies, `expected` should contain a pair of the declaration name and an empty set.
[[nodiscard]] bool test_dependencies_match(std::string_view file,
                                           const Dependency_Map& expected,
                                           Dependency_Search_Type search_type)
{
    return test_for_success_then_introspect(file, [&](bms::Analyzed_Program& program) {
        const Dependency_Map actual
            = gather_all_dependencies(program, expected.get_allocator().resource(), search_type);
        return actual == expected;
    });
}

struct [[nodiscard]] Dependency_Map_Builder {
    Dependency_Map result;

    explicit Dependency_Map_Builder(std::pmr::memory_resource* memory)
        : result { memory }
    {
    }

    Dependency_Map_Builder& insert(std::string_view key,
                                   std::initializer_list<Dependency_By_Name> dependencies)
    {
        auto [iter, success] = result.emplace(
            key, Dependency_Set { dependencies, result.get_allocator().resource() });
        BIT_MANIPULATION_ASSERT(success);
        return *this;
    }

    Dependency_Map_Builder& insert(std::string_view key,
                                   std::initializer_list<std::string_view> names,
                                   bmd::Dependency_Type type)
    {
        auto dependencies = names | std::views::transform([type](std::string_view name) {
                                return Dependency_By_Name { name, type };
                            });

        Dependency_Set value { dependencies.begin(), dependencies.end(), 0,
                               result.get_allocator().resource() };
        auto [iter, success] = result.emplace(key, std::move(value));
        BIT_MANIPULATION_ASSERT(success);
        return *this;
    }

    Dependency_Map_Builder& insert_empty(std::string_view key)
    {
        auto [iter, success]
            = result.emplace(key, Dependency_Set { result.get_allocator().resource() });
        BIT_MANIPULATION_ASSERT(success);
        return *this;
    }

    [[nodiscard]] Dependency_Map build()
    {
        return std::move(result);
    }
};

TEST(Dependencies, none)
{
    std::pmr::monotonic_buffer_resource memory;
    const auto expected = Dependency_Map_Builder { &memory } //
                              .insert_empty("awoo")
                              .insert_empty("chan")
                              .build();
    ASSERT_TRUE(test_dependencies_match("dependencies/none.bms", expected,
                                        Dependency_Search_Type::functions_recursively));
}

TEST(Dependencies, direct_recursion)
{
    constexpr std::string_view file = "dependencies/direct_recursion.bms";

    std::pmr::monotonic_buffer_resource memory;
    Dependency_Set awoo_dependencies;
    const auto expected_direct
        = Dependency_Map_Builder { &memory } //
              .insert("awoo", { "awoo" }, bmd::Dependency_Type::normal_direct)
              .build();
    EXPECT_TRUE(
        test_dependencies_match(file, expected_direct, Dependency_Search_Type::direct_functions));

    const auto expected_recursive
        = Dependency_Map_Builder { &memory } //
              .insert("awoo",
                      { { "awoo", bmd::Dependency_Type::normal_direct },
                        { "awoo", bmd::Dependency_Type::normal_recursive } })
              .build();
    EXPECT_TRUE(test_dependencies_match(file, expected_recursive,
                                        Dependency_Search_Type::functions_recursively));
}

TEST(Dependencies, mutual_recursion)
{
    constexpr std::string_view file = "dependencies/mutual_recursion.bms";

    std::pmr::monotonic_buffer_resource memory;

    const auto expected_recursive
        = Dependency_Map_Builder { &memory }
              .insert("awoo",
                      { { "awoo", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_direct } })
              .insert("chan",
                      { { "awoo", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_recursive },
                        { "awoo", bmd::Dependency_Type::normal_direct } })
              .build();
    ASSERT_TRUE(test_dependencies_match(file, expected_recursive,
                                        Dependency_Search_Type::functions_recursively));
}

TEST(Dependencies, mutual_recursion_3)
{
    constexpr std::string_view file = "dependencies/mutual_recursion_3.bms";

    std::pmr::monotonic_buffer_resource memory;
    const auto expected_recursive
        = Dependency_Map_Builder { &memory }
              .insert("awoo",
                      { { "awoo", bmd::Dependency_Type::normal_recursive },
                        { "baka", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_recursive },
                        { "baka", bmd::Dependency_Type::normal_direct } })
              .insert("baka",
                      { { "awoo", bmd::Dependency_Type::normal_recursive },
                        { "baka", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_direct } })
              .insert("chan",
                      { { "awoo", bmd::Dependency_Type::normal_recursive },
                        { "baka", bmd::Dependency_Type::normal_recursive },
                        { "chan", bmd::Dependency_Type::normal_recursive },
                        { "awoo", bmd::Dependency_Type::normal_direct } })
              .build();
    ASSERT_TRUE(test_dependencies_match(file, expected_recursive,
                                        Dependency_Search_Type::functions_recursively));
}

TEST(Dependencies, constant_direct)
{
    constexpr std::string_view file = "dependencies/constant_direct.bms";

    std::pmr::monotonic_buffer_resource memory;
    const auto expected_recursive
        = Dependency_Map_Builder { &memory } //
              .insert("awoo", { "zero" }, bmd::Dependency_Type::constant_direct)
              .insert_empty("zero")
              .build();
    ASSERT_TRUE(test_dependencies_match(file, expected_recursive,
                                        Dependency_Search_Type::functions_recursively));
}

TEST(Dependencies, constant_recursive)
{
    constexpr std::string_view file = "dependencies/constant_recursive.bms";

    std::pmr::monotonic_buffer_resource memory;
    const auto expected_recursive
        = Dependency_Map_Builder { &memory } //
              .insert("awoo",
                      { { "zero", bmd::Dependency_Type::normal_direct },
                        { "zero_impl", bmd::Dependency_Type::constant_recursive } })
              .insert("zero", { { "zero_impl", bmd::Dependency_Type::constant_direct } })
              .insert_empty("zero_impl")
              .build();
    ASSERT_TRUE(test_dependencies_match(file, expected_recursive,
                                        Dependency_Search_Type::functions_recursively));
}

} // namespace
} // namespace bit_manipulation
