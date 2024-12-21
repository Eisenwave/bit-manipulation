#ifndef BIT_MANIPULATION_FUNCTION_REF_HPP
#define BIT_MANIPULATION_FUNCTION_REF_HPP

#include <concepts>

#include "common/assert.hpp"

namespace bit_manipulation {

template <typename F>
struct Function_Ref;

template <typename R, typename... Args>
struct Function_Ref<R(Args...)> {
private:
    void* m_entity = nullptr;
    R (*m_invoker)(void*, Args...) = nullptr;

public:
    Function_Ref() = default;

    template <typename F>
        requires(!std::same_as<F, Function_Ref>)
    [[nodiscard]] Function_Ref(F& f) noexcept
        : m_entity { std::addressof(f) }
        , m_invoker { [](void* entity, Args... args) -> R {
            return (*static_cast<F*>(entity))(std::forward<Args>(args)...);
        } }
    {
    }

    R operator()(Args... args) const
    {
        BIT_MANIPULATION_ASSERT(m_entity);
        BIT_MANIPULATION_ASSERT(m_invoker);
        return m_invoker(m_entity, std::forward<Args>(args)...);
    }
};

} // namespace bit_manipulation

#endif
