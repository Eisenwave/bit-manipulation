#ifndef BIT_MANIPULATION_BMD_HTML_TOKEN_CONSUMER_HPP
#define BIT_MANIPULATION_BMD_HTML_TOKEN_CONSUMER_HPP

#include <string_view>

#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd {

/// @brief The polymorphic token consumer.
/// This class' member functions are invoked by `HTML_Writer` with the text content and the
/// token type to write.
/// The token type can be ignored, but may be used to output with syntax highlighting.
struct HTML_Token_Consumer {

    /// @brief Consumes a single character `c` with the given `type`.
    virtual bool write(char c, HTML_Token_Type type) = 0;

    /// @brief Consumes `n` repetitions of a character `c` with the given `type`.
    /// This is typically used for indentation.
    virtual bool write(char c, Size n, HTML_Token_Type type) = 0;

    /// @brief Consumes a string `s` with the given `type`.
    virtual bool write(std::string_view s, HTML_Token_Type type) = 0;
};

/// @brief A convenience base class for token consumer which ignore the
/// given token types and consume only the character data.
struct Data_HTML_Token_Consumer : HTML_Token_Consumer {

    bool write(char c, HTML_Token_Type) final
    {
        return write(c);
    }

    bool write(char c, Size n, HTML_Token_Type) final
    {
        return write(c, n);
    }

    bool write(std::string_view s, HTML_Token_Type) final
    {
        return write(s);
    }

    virtual bool write(char c) = 0;
    virtual bool write(char c, Size n) = 0;
    virtual bool write(std::string_view s) = 0;
};

} // namespace bit_manipulation::bmd

#endif
