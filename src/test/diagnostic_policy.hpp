#ifndef BIT_MANIPULATION_DIAGNOSTIC_POLICY_HPP
#define BIT_MANIPULATION_DIAGNOSTIC_POLICY_HPP

#include <string_view>

#include "common/io_error.hpp"

#include "bms/fwd.hpp"

#include "bmd/fwd.hpp"

#include "test/compilation_stage.hpp"

namespace bit_manipulation {

enum struct Policy_Action {
    /// @brief Immediate success.
    success,
    /// @brief Immediate failure.
    failure,
    /// @brief Keep going.
    keep_going
};

constexpr bool is_exit(Policy_Action action)
{
    return action != Policy_Action::keep_going;
}

/// @brief A polymorphic class for deciding which `Policy_Action` to take when various diagnostics
/// are raised throughout testing.
/// Diagnostic policies are stateful, i.e. they are required to remember failures and keep these
/// consistent with `is_success()`.
struct BMS_Diagnostic_Policy {
    /// @brief Returns `false` if any prior call returned `Policy_Action::failure`,
    /// returns `true` if any prior call returned `Policy_Action::success`,
    /// or some other value if the policy is otherwise not considered to have succeeded.
    virtual bool is_success() const = 0;

    virtual Policy_Action error(IO_Error_Code) = 0;
    virtual Policy_Action error(const bms::Tokenize_Error&) = 0;
    virtual Policy_Action error(const bms::Parse_Error&) = 0;
    virtual Policy_Action error(const bms::Analysis_Error&) = 0;

    virtual Policy_Action done(BMS_Stage) = 0;
};

struct BMD_Diagnostic_Policy {
    virtual bool is_success() const = 0;

    virtual Policy_Action error(IO_Error_Code) = 0;
    virtual Policy_Action error(const bmd::Parse_Error&) = 0;
    virtual Policy_Action error(const bmd::Document_Error&) = 0;

    virtual Policy_Action done(BMD_Stage) = 0;
};

} // namespace bit_manipulation

#endif
