#ifndef BIT_MANIPULATION_FUNCTION_REF_HPP
#define BIT_MANIPULATION_FUNCTION_REF_HPP

#include <concepts>
#include <utility>

#include "common/assert.hpp"
#include "common/meta.hpp"

namespace bit_manipulation {

template <typename T>
concept function_pointer = std::is_pointer_v<T> && std::is_function_v<std::remove_pointer_t<T>>;

template <typename T, typename R, typename... Args>
concept invocable_r
    = std::invocable<T, Args...> && std::same_as<std::invoke_result_t<T, Args...>, R>;

template <typename T, typename... Args>
concept nothrow_invocable = std::invocable<T, Args...> && std::is_nothrow_invocable_v<T, Args...>;

template <typename T, typename R, typename... Args>
concept nothrow_invocable_r = invocable_r<T, R, Args...> && nothrow_invocable<T, Args...>;

template <typename T, bool nothrow, typename R, typename... Args>
concept invocable_n_r
    = invocable_r<T, R, Args...> && (!nothrow || nothrow_invocable_r<T, R, Args...>);

template <bool constant, bool nothrow, typename R, typename... Args>
struct Function_Ref_Base {
private:
    using Function_Pointer_Type = R (*)(Args...) noexcept(nothrow);
    using Storage_Type = const_if_t<void, constant>*;

    template <typename F>
        requires std::is_pointer_v<F>
    static R call(Storage_Type entity, Args... args) noexcept(nothrow)
    {
        if constexpr (std::is_function_v<std::remove_pointer_t<F>>) {
            // This 'const_cast' is needed because Clang does not support conversions from
            // 'const void*' to function pointer types.
            // This could be considered a bug or a language defect
            // (see https://github.com/cplusplus/CWG/issues/657).
            // In any case, we need to remove 'const'.
            void* entity_raw = const_cast<void*>(entity);
            return R((*reinterpret_cast<F>(entity_raw))(std::forward<Args>(args)...));
        }
        else {
            return R((*reinterpret_cast<F>(entity))(std::forward<Args>(args)...));
        }
    }

    R (*m_invoker)(Storage_Type, Args...) noexcept(nothrow) = nullptr;
    Storage_Type m_entity = nullptr;

public:
    [[nodiscard]] constexpr Function_Ref_Base() = default;

    /// @brief Constructs a `Function_Ref` from a compile-time constant which is convertible
    /// to a function pointer.
    ///
    /// This will create a `Function_Ref` which is bound to nothing,
    /// and when called, simply forwards to `F`.
    template <std::convertible_to<Function_Pointer_Type> auto F>
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
    template <typename F>
        requires(!std::same_as<std::remove_cvref_t<F>, Function_Ref_Base>
                 && invocable_n_r<follow_ref_const_if_t<F, constant>, nothrow, R, Args...>)
    [[nodiscard]] constexpr Function_Ref_Base(F&& f) noexcept
    {
        using Entity = std::remove_reference_t<F>;

        if constexpr (std::is_function_v<Entity>) {
            m_invoker = &call<Entity* const>;
            m_entity = reinterpret_cast<Storage_Type>(&f);
        }
        else if constexpr (function_pointer<Entity>) {
            m_invoker = &call<const Entity>;
            m_entity = reinterpret_cast<Storage_Type>(f);
        }
        else if constexpr (std::is_convertible_v<F&&, Function_Pointer_Type>) {
            const Function_Pointer_Type pointer = f;
            m_invoker = &call<decltype(pointer)>;
            m_entity = reinterpret_cast<Storage_Type>(pointer);
        }
        else if constexpr (requires {
                               { +f } -> function_pointer;
                           }) {
            // This case covers e.g. captureless lambdas.
            // Those can always be converted to function pointers, but not exactly to
            // Function_Pointer_Type; that case has already been handled above.
            auto pointer = +f;
            m_invoker = &call<decltype(pointer)>;
            m_entity = reinterpret_cast<Storage_Type>(pointer);
        }
        else {
            m_invoker = &call<const_if_t<Entity, constant>*>;
            m_entity = std::addressof(f);
        }
    }

    constexpr R operator()(Args... args) const noexcept(nothrow)
    {
        BIT_MANIPULATION_ASSERT(m_entity);
        BIT_MANIPULATION_ASSERT(m_invoker);
        return m_invoker(m_entity, std::forward<Args>(args)...);
    }

    [[nodiscard]] constexpr bool has_value() const noexcept
    {
        return m_invoker != nullptr;
    }

    [[nodiscard]] constexpr operator bool() const noexcept
    {
        return has_value();
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
