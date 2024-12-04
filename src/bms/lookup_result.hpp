#ifndef BIT_MANIPULATION_LOOKUP_RESULT_HPP
#define BIT_MANIPULATION_LOOKUP_RESULT_HPP

#include <string_view>

#include "common/variant.hpp"

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

using Lookup_Result_Variant = Variant<ast::Some_Node*, ast::Parameter*, Builtin_Function>;

struct Lookup_Result : Lookup_Result_Variant {
    using Variant::Variant;

    operator Optional_Lookup_Result() const;
};

using Optional_Lookup_Result_Variant
    = Variant<Monostate, ast::Some_Node*, ast::Parameter*, Builtin_Function>;

struct Optional_Lookup_Result : Optional_Lookup_Result_Variant {
    using Variant::Variant;

    Optional_Lookup_Result() = default;

    explicit operator bool() const
    {
        return index() != 0;
    }

    Lookup_Result operator*() const
    {
        BIT_MANIPULATION_ASSERT(*this);
        return visit(
            []<typename T>(const T& self) -> Lookup_Result {
                if constexpr (std::is_same_v<T, Monostate>) {
                    BIT_MANIPULATION_ASSERT_UNREACHABLE("Cannot be monostate.");
                }
                else {
                    return Lookup_Result { self };
                }
            },
            *this);
    }

    [[nodiscard]] Lookup_Result value() const
    {
        return **this;
    }
};

inline Lookup_Result::operator Optional_Lookup_Result() const
{
    return visit([](const auto& self) { return Optional_Lookup_Result { self }; }, *this);
}

static_assert(std::is_trivially_copyable_v<Lookup_Result>);
static_assert(std::is_trivially_copyable_v<Optional_Lookup_Result>);

} // namespace bit_manipulation::bms

#endif
