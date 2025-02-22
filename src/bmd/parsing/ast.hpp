#ifndef BIT_MANIPULATION_BMD_AST_HPP
#define BIT_MANIPULATION_BMD_AST_HPP

#include <memory_resource>
#include <span>
#include <string_view>
#include <vector>

#include "common/meta.hpp"
#include "common/source_position.hpp"
#include "common/variant.hpp"

#include "bmd/directive_type.hpp"
#include "bmd/fwd.hpp"

namespace bit_manipulation::bmd::ast {

namespace detail {
struct Base {
    Local_Source_Span m_pos;

    [[nodiscard]] Local_Source_Span get_source_position() const
    {
        return m_pos;
    }
};

} // namespace detail

struct Argument final : detail::Base {
private:
    std::pmr::vector<Content> m_content;
    Local_Source_Span m_name;

public:
    [[nodiscard]] Argument(const Local_Source_Span& pos,
                           const Local_Source_Span& name,
                           std::pmr::vector<ast::Content>&& children);

    [[nodiscard]] Argument(const Local_Source_Span& pos, std::pmr::vector<ast::Content>&& children);

    ~Argument();

    [[nodiscard]] bool has_name() const
    {
        return !m_name.empty();
    }
    [[nodiscard]] Local_Source_Span get_name_span() const;
    [[nodiscard]] std::string_view get_name(std::string_view source) const
    {
        BIT_MANIPULATION_ASSERT(m_name.begin + m_name.length <= source.size());
        return source.substr(m_name.begin, m_name.length);
    }

    [[nodiscard]] std::span<Content> get_content() &;
    [[nodiscard]] std::span<const Content> get_content() const&;
    [[nodiscard]] std::pmr::vector<Content>&& get_content() &&;
};

struct Directive final : detail::Base {
private:
    Size m_name_length;

    std::pmr::vector<Argument> m_arguments;
    std::pmr::vector<Content> m_content;

public:
    [[nodiscard]] Directive(const Local_Source_Span& pos,
                            Size name_length,
                            std::pmr::vector<Argument>&& args,
                            std::pmr::vector<Content>&& block);

    ~Directive();

    [[nodiscard]] std::string_view get_name(std::string_view source) const
    {
        return source.substr(m_pos.begin + 1, m_name_length);
    }

    [[nodiscard]] std::span<Argument> get_arguments();
    [[nodiscard]] std::span<const Argument> get_arguments() const;
    [[nodiscard]] std::span<Content> get_content();
    [[nodiscard]] std::span<Content const> get_content() const;
};

struct Text final : detail::Base {

    [[nodiscard]] Text(const Local_Source_Span& pos);

    [[nodiscard]] std::string_view get_text(std::string_view source) const
    {
        return source.substr(m_pos.begin, m_pos.length);
    }
};

struct Content : Variant<Directive, Text> {
    using Variant::Variant;
};

inline Argument::~Argument() = default;
inline Directive::~Directive() = default;

inline std::span<Content> Argument::get_content() &
{
    return m_content;
}
inline std::span<const Content> Argument::get_content() const&
{
    return m_content;
}
inline std::pmr::vector<Content>&& Argument::get_content() &&
{
    return std::move(m_content);
}

inline std::span<Argument> Directive::get_arguments()
{
    return m_arguments;
}
inline std::span<const Argument> Directive::get_arguments() const
{
    return m_arguments;
}

inline std::span<Content> Directive::get_content()
{
    return m_content;
}
inline std::span<Content const> Directive::get_content() const
{
    return m_content;
}

[[nodiscard]] inline Local_Source_Span get_source_span(const Content& node)
{
    return visit([]<typename T>(const T& v) -> const detail::Base& { return v; }, node)
        .get_source_position();
}

template <bool constant>
struct Visitor_Impl {
    using Argument_Type = const_if_t<Argument, constant>;
    using Text_Type = const_if_t<Text, constant>;
    using Directive_Type = const_if_t<Directive, constant>;
    using Content_Type = const_if_t<Content, constant>;

    void visit_arguments(Directive_Type& directive)
    {
        for (Argument_Type& arg : directive.get_arguments()) {
            this->visit(arg);
        }
    }

    void visit_children(Directive_Type& directive)
    {
        visit_arguments();
        visit_content_sequence(directive.get_content());
    }

    void visit_children(Argument_Type& argument)
    {
        visit_content_sequence(argument.get_content());
    }

    void visit_content(Content_Type& content)
    {
        if (auto* d = get_if<Directive>(&content)) {
            this->visit(*d);
        }
        else {
            this->visit(get<Text>(content));
        }
    }

    void visit_content_sequence(std::span<Content_Type> content)
    {
        for (Content_Type& c : content) {
            this->visit_content(c);
        }
    }

    virtual void visit(Directive_Type& directive) = 0;
    virtual void visit(Text_Type& text) = 0;
    virtual void visit(Argument_Type& argument) = 0;
};

using Visitor = Visitor_Impl<false>;
using Const_Visitor = Visitor_Impl<true>;

} // namespace bit_manipulation::bmd::ast

#endif
