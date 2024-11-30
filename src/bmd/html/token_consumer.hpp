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
    virtual bool write(char c, HTML_Token_Type type) = 0;
    virtual bool write(char c, Size n, HTML_Token_Type type) = 0;
    virtual bool write(std::string_view s, HTML_Token_Type type) = 0;
};

} // namespace bit_manipulation::bmd

#endif
