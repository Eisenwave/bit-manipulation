#ifndef BIT_MANIPULATION_FUNCTION_REF_HPP
#define BIT_MANIPULATION_FUNCTION_REF_HPP

#include <concepts>
#include <utility>

#include "common/assert.hpp"
#include "common/meta.hpp"

namespace bit_manipulation {

template <typename T, bool nothrow, typename R, typename... Args>
concept invocable_n_r = (nothrow && std::is_nothrow_invocable_r_v<R, T, Args...>)
    || (!nothrow && std::is_invocable_r_v<R, T, Args...>);

template <bool constant, bool nothrow, typename R, typename... Args>
struct Function_Ref_Base {
private:
    using Function_Pointer = R (*)(Args...) noexcept(nothrow);
    using Storage_Type = const_if_t<void, constant>*;

    template <typename F>
        requires std::is_pointer_v<F>
    struct Invoker {
        static R call(Storage_Type entity, Args... args) noexcept(nothrow)
        {
            return (*reinterpret_cast<F>(entity))(std::forward<Args>(args)...);
        }
    };

    R (*m_invoker)(Storage_Type, Args...) noexcept(nothrow) = nullptr;
    Storage_Type m_entity = nullptr;

public:
    [[nodiscard]] constexpr Function_Ref_Base() = default;

    /// @brief Constructs a `Function_Ref` from a compile-time constant which is convertible
    /// to a function pointer.
    ///
    /// This will create a `Function_Ref` which is bound to nothing,
    /// and when called, simply forwards to `F`.
    template <std::convertible_to<Function_Pointer> auto F>
    [[nodiscard]] consteval Function_Ref_Base(Constant<F>) noexcept
        : m_invoker { [](Storage_Type, Args... args) noexcept(nothrow) { //
            return F(std::forward<Args>(args)...);
        } }
    {
    }

    /// @brief Constructs a `Function_Ref` from some callable type.
    ///
    /// If `f` is convertible to `R(*)(Args...)`,
    /// this constructor accepts both lvalues and rvalues.
    /// This applies in the case of function references, function pointers, captureless lambdas,
    /// etc.
    /// The `Function_Ref` will bind to the given function pointer in such a case.
    ///
    /// Otherwise, only lvalues are accepted, and the `Function_Ref` binds to `f`.
    template <invocable_n_r<nothrow, R, Args...> F>
        requires(!std::same_as<std::remove_cvref_t<F>, Function_Ref_Base>)
    [[nodiscard]] constexpr Function_Ref_Base(F&& f) noexcept
    {
        using Entity = std::remove_reference_t<F>;

        if constexpr (std::is_convertible_v<F&&, Function_Pointer>) {
            const Function_Pointer pointer = f;
            m_invoker = &Invoker<decltype(pointer)>::call;
            m_entity = reinterpret_cast<Storage_Type>(pointer);
        }
        else {
            static_assert(std::is_lvalue_reference_v<F&&>,
                          "Function_Ref cannot bind to rvalues of lambdas with captures, or to "
                          "other function objects that cannot be converted to function pointers.");
            static_assert(std::is_invocable_v<const_if_t<Entity, constant>&, Args...>,
                          "This Function_Ref has a const qualifier on the function type, so it can "
                          "only bind to function pointers, or to entities with a const call "
                          "operator. Did you forget to mark your call operator 'const'?");
            m_invoker = &Invoker<Entity*>::call;
            m_entity = std::addressof(f);
        }
    }

    constexpr R operator()(Args... args) const noexcept(nothrow)
    {
        BIT_MANIPULATION_ASSERT(m_entity);
        BIT_MANIPULATION_ASSERT(m_invoker);
        return m_invoker(m_entity, std::forward<Args>(args)...);
    }
};

template <typename F>
struct Function_Ref;

template <typename R, typename... Args>
struct Function_Ref<R(Args...)> : Function_Ref_Base<false, false, R, Args...> {
    using Function_Ref_Base<false, false, R, Args...>::Function_Ref_Base;
};

template <typename R, typename... Args>
struct Function_Ref<R(Args...) noexcept> : Function_Ref_Base<false, true, R, Args...> {
    using Function_Ref_Base<false, true, R, Args...>::Function_Ref_Base;
};

template <typename R, typename... Args>
struct Function_Ref<R(Args...) const> : Function_Ref_Base<true, false, R, Args...> {
    using Function_Ref_Base<true, false, R, Args...>::Function_Ref_Base;
};

template <typename R, typename... Args>
struct Function_Ref<R(Args...) const noexcept> : Function_Ref_Base<true, true, R, Args...> {
    using Function_Ref_Base<true, true, R, Args...>::Function_Ref_Base;
};

} // namespace bit_manipulation

#endif
