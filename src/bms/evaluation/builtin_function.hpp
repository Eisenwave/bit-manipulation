#ifndef BIT_MANIPULATION_BUILTIN_FUNCTION_HPP
#define BIT_MANIPULATION_BUILTIN_FUNCTION_HPP

#include <span>

#include "bms/concrete_type.hpp"

namespace bit_manipulation::bms {

enum struct Builtin_Function : Default_Underlying {
    // `fn assert(cond: Bool) -> Void`
    assert
};

[[nodiscard]] constexpr Size builtin_parameter_count(Builtin_Function f)
{
    using enum Builtin_Function;
    switch (f) {
    case assert: return 1;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function");
}

[[nodiscard]] constexpr Concrete_Type builtin_return_type(Builtin_Function f)
{
    using enum Builtin_Function;
    switch (f) {
    case assert: return Concrete_Type::Void;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function");
}

namespace detail {

template <Builtin_Function F>
inline constexpr std::nullptr_t builtin_parameters_v = nullptr;

template <>
inline constexpr Concrete_Type builtin_parameters_v<Builtin_Function::assert>[1] {
    Concrete_Type::Bool
};

} // namespace detail

[[nodiscard]] constexpr std::span<const Concrete_Type> builtin_parameter_types(Builtin_Function f)
{
    using enum Builtin_Function;
    switch (f) {
    case assert: return detail::builtin_parameters_v<Builtin_Function::assert>;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("unknown builtin function");
}

} // namespace bit_manipulation::bms

#endif
