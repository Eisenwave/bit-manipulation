#ifndef BIT_MANIPULATION_DIAGNOSTIC_CONSUMER_HPP
#define BIT_MANIPULATION_DIAGNOSTIC_CONSUMER_HPP

#include "bms/fwd.hpp"

namespace bit_manipulation::bms {

enum struct Error_Reaction : Default_Underlying {
    abort,
    keep_going,
};

/// @brief A consumer for diagnostics that can be used throughout all stages of compilation.
struct Diagnostic_Consumer {
    // The lack of virtual destructor is intentional; we don't ever store this polymorphically.

    [[nodiscard]] virtual Error_Reaction operator()(Tokenize_Error&&) = 0;
    [[nodiscard]] virtual Error_Reaction operator()(Parse_Error&&) = 0;
    [[nodiscard]] virtual Error_Reaction operator()(Analysis_Error&&) = 0;

    /// @brief Returns the total amount of errors.
    virtual Size error_count() const noexcept = 0;

    /// @brief Removes all collected diagnostics from the consumer.
    ///
    /// Postcondition: `error_count()` is zero.
    virtual void clear() noexcept = 0;

    /// @brief Equivalent to: `error_count() == 0`
    bool ok() const noexcept
    {
        return error_count() == 0;
    }
};

} // namespace bit_manipulation::bms

#endif
