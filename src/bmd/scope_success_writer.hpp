#ifndef BIT_MANIPULATION_BMD_SCOPE_SUCCESS_WRITER_HPP
#define BIT_MANIPULATION_BMD_SCOPE_SUCCESS_WRITER_HPP

#include <exception>
#include <iosfwd>
#include <string_view>
#include <utility>

namespace bit_manipulation::bmd {

struct [[nodiscard]] Scope_Success_Writer {
private:
    std::ostream* m_out;
    std::string_view m_data;

public:
    Scope_Success_Writer(std::ostream& out, std::string_view data)
        : m_out(&out)
        , m_data(data)
    {
    }

    ~Scope_Success_Writer()
    {
        if (m_out != nullptr && std::uncaught_exceptions() == 0) {
            *m_out << m_data;
        }
    }

    Scope_Success_Writer(const Scope_Success_Writer&) = delete;
    Scope_Success_Writer(Scope_Success_Writer&& other) noexcept
        : m_out(std::exchange(other.m_out, nullptr))
        , m_data(other.m_data)
    {
    }

    Scope_Success_Writer& operator=(const Scope_Success_Writer&) = delete;
    Scope_Success_Writer& operator=(Scope_Success_Writer&& other) noexcept
    {
        swap(*this, other);
        other.m_out = nullptr;
        return *this;
    }

    friend void swap(Scope_Success_Writer& x, Scope_Success_Writer& y) noexcept
    {
        std::swap(x.m_out, y.m_out);
        std::swap(x.m_data, y.m_data);
    }
};

} // namespace bit_manipulation::bmd

#endif
