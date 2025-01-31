#ifndef BIT_MANIPULATION_BMD_DECLARATIONS_HPP
#define BIT_MANIPULATION_BMD_DECLARATIONS_HPP

#include "common/config.hpp"
#include "common/function_ref.hpp"

#include "bms/ast.hpp"

namespace bit_manipulation::bmd {

enum struct Dependency_Type : Default_Underlying {
    /// @brief A regular dependency, such as between a function and the functions
    /// it calls at runtime.
    normal_direct,
    /// @brief Like `regular`, but indirect dependency
    /// (e.g. `a()` calls `b()` and `b()` calls `c()`, so `c` is a dependency of `a`).
    normal_recursive,
    /// @brief A dependency needed in a constant expression, such as the dependency
    /// between `Uint(x)` and the constant `x`.
    ///
    /// However, this does not include dependencies such as if `awoo()` calls `chan()` and there
    /// exists a constant expression `Uint(awoo())`,
    /// i.e. "constant dependencies" that are only indirectly needed for constant evaluation.
    /// In that example, `chan` would be `normal_recursive`,
    /// unless `chan()` is directly needed by a constant expression within `awoo()`,
    /// and only then is `chan` `constant_recursive`.
    ///
    /// This kind of classification ensures that nothing can have a constant dependency on itself
    /// in a well-formed program, and that recursive dependencies can be formed by
    /// taking the union of direct dependencies and the direct dependencies of all the dependents
    /// recursively,
    /// albeit with any `direct` dependency replaced with a `recursive` one.
    constant_direct,
    /// @brief Indirect constant dependency.
    constant_recursive,
};

[[nodiscard]] constexpr bool dependency_type_is_normal(Dependency_Type type)
{
    return type == Dependency_Type::normal_direct || type == Dependency_Type::normal_recursive;
}

[[nodiscard]] constexpr bool dependency_type_is_recursive(Dependency_Type type)
{
    return type == Dependency_Type::normal_recursive || type == Dependency_Type::constant_recursive;
}

struct Dependency {
    const bms::ast::Some_Node* declaration;
    Dependency_Type type;

    friend std::strong_ordering operator<=>(const Dependency&, const Dependency&) = default;
};

/// @brief Collects the dependencies on global declarations that a certain node has, recursively.
/// For example, for a given function, this will invoke `out` for each function or global constant
/// this is used within that function.
///
/// The `out` visitor also controls whether the given dependency should be examined recursively.
/// A typical approach may include the use of a `std::unordered_set` where visitor indicates
/// if `emplace` succeeded.
/// @param out invoked for each dependency
/// @param node the visited node, which is usually a function or global constant in itself
void for_each_global_dependency(Function_Ref<bool(Dependency)> out,
                                const bms::ast::Some_Node& node);

/// @brief Convenience function which calls `for_each_global_dependency`
/// and never examines dependencies recursively.
/// @param out invoked for each dependency
/// @param node the visited node, which is usually a function or global constant in itself
void for_each_direct_global_dependency(Function_Ref<void(Dependency)> out,
                                       const bms::ast::Some_Node& node);

} // namespace bit_manipulation::bmd

template <>
struct std::hash<bit_manipulation::bmd::Dependency> {
    [[nodiscard]] std::size_t operator()(const bit_manipulation::bmd::Dependency& d) const
    {
        return std::hash<const void*> {}(d.declaration)
            ^ std::hash<bit_manipulation::Default_Underlying> {}(
                   bit_manipulation::Default_Underlying(d.type));
    }
};

#endif
