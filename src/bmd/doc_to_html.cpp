#include <algorithm>
#include <unordered_map>

#include "bmd/doc_to_html.hpp"
#include "bmd/html_writer.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation::bmd {

namespace {

struct Tag_Base_Info {
    /// @brief The formatting style for this tag.
    Formatting_Style style;
    /// @brief `true` if the tag is an HTML void tag (`<br>`, `<hr>`, etc.) which must always
    /// be empty.
    bool is_void;
};

std::optional<Tag_Base_Info> identify_passthrough_tag(std::string_view directive_id)
{
    BIT_MANIPULATION_ASSERT(!directive_id.empty());

    static const std::unordered_map<std::string_view, Tag_Base_Info> lookup {
        { "b", { Formatting_Style::in_line, false } },
        { "br", { Formatting_Style::in_line, true } },
        { "del", { Formatting_Style::in_line, false } },
        { "h1", { Formatting_Style::block, false } },
        { "h2", { Formatting_Style::block, false } },
        { "h3", { Formatting_Style::block, false } },
        { "h4", { Formatting_Style::block, false } },
        { "h5", { Formatting_Style::block, false } },
        { "h6", { Formatting_Style::block, false } },
        { "hr", { Formatting_Style::flat, true } },
        { "i", { Formatting_Style::in_line, false } },
        { "kbd", { Formatting_Style::in_line, false } },
        { "ol", { Formatting_Style::block, false } },
        { "q", { Formatting_Style::in_line, false } },
        { "s", { Formatting_Style::in_line, false } },
        { "sub", { Formatting_Style::in_line, false } },
        { "sup", { Formatting_Style::in_line, false } },
        { "tt", { Formatting_Style::in_line, false } },
        { "u", { Formatting_Style::in_line, false } },
        { "ul", { Formatting_Style::block, false } },
    };

    const auto it = lookup.find(directive_id);
    if (it == lookup.end()) {
        return {};
    }
    return it->second;
}

struct Tag_Info : Tag_Base_Info {
    std::string_view tag;
    bool is_passthrough;

    [[nodiscard]] explicit operator bool() const
    {
        return !tag.empty();
    }
};

Tag_Info tag_of(const ast::Directive& directive)
{
    const std::string_view id = directive.get_identifier();

    if (std::optional<Tag_Base_Info> passthrough = identify_passthrough_tag(id)) {
        return { *passthrough, id, true };
    }

    return {};
}

struct HTML_Converter {
    HTML_Writer& m_writer;

    explicit HTML_Converter(HTML_Writer& writer)
        : m_writer { writer }
    {
    }

    [[nodiscard]] Result<void, Document_Error> operator()(const ast::Content& content,
                                                          Formatting_Style inherited_style)
    {
        for (const ast::Some_Node* const p : content.get_children()) {
            auto r = operator()(std::get<ast::Paragraph>(*p), inherited_style);
            if (!r) {
                return r;
            }
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> operator()(const ast::Paragraph& paragraph,
                                                          Formatting_Style)
    {
        m_writer.begin_tag("p", Formatting_Style::block);
        for (const ast::Some_Node* const n : paragraph.get_children()) {
            if (const auto* const text = std::get_if<ast::Text>(n)) {
                if (auto r = operator()(*text, Formatting_Style::block); !r) {
                    return r;
                }
                continue;
            }
            if (const auto* const directive = std::get_if<ast::Directive>(n)) {
                if (auto r = operator()(*directive, Formatting_Style::block); !r) {
                    return r;
                }
                continue;
            }
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Paragraphs should only contain text or directives.");
        }
        m_writer.end_tag("p", Formatting_Style::block);
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> operator()(const ast::Directive& directive,
                                                          Formatting_Style)
    {
        const Tag_Info info = tag_of(directive);
        if (!info) {
            return Document_Error { Document_Error_Code::unknown_directive,
                                    directive.get_source_position() };
        }

        if (!directive.m_arguments.empty()) {
            BIT_MANIPULATION_ASSERT(info.is_passthrough); // otherwise not implemented yet
            auto attribute_writer = m_writer.begin_tag_with_attributes(info.tag, info.style);
            for (const auto& [key, value] : directive.m_arguments) {
                if (const auto* const text = std::get_if<ast::Identifier>(&value)) {
                    attribute_writer.write_attribute(key, text->get_value());
                    continue;
                }
                if (const auto* const text = std::get_if<ast::Number>(&value)) {
                    attribute_writer.write_attribute(key, std::to_string(text->get_value()));
                    continue;
                }
                BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown attribute value type.");
            }
            if (directive.get_block() == nullptr) {
                attribute_writer.end_empty();
            }
            else {
                attribute_writer.end();
            }
        }

        if (directive.get_block() == nullptr) {
            m_writer.write_empty_tag(info.tag, info.style);
        }
        else {
            m_writer.begin_tag(info.tag, info.style);
            if (auto r = operator()(directive.get_block(), info.style); !r) {
                return r;
            }
            m_writer.end_tag(info.tag, info.style);
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> operator()(const ast::Text& text,
                                                          Formatting_Style inherited_style)
    {
        m_writer.write_inner_text(text.get_text(), inherited_style);
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> operator()(const ast::Some_Node* node,
                                                          Formatting_Style inherited_style)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        return fast_visit(
            [this, inherited_style](const auto& n) { return operator()(n, inherited_style); },
            *node);
    }
};

} // namespace

Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out, const Parsed_Document& document)
{
    HTML_Writer writer { out };
    writer.write_preamble();

    writer.begin_tag("html", Formatting_Style::flat);
    writer.begin_tag("body", Formatting_Style::flat);

    if (document.root_node != nullptr) {
        auto r = HTML_Converter { writer }(document.root_node, Formatting_Style::flat);
        if (!r) {
            return r;
        }
    }

    writer.end_tag("body", Formatting_Style::flat);
    writer.end_tag("html", Formatting_Style::flat);
    if (!writer.is_done()) {
        return Document_Error { Document_Error_Code::writer_misuse, {} };
    }
    return {};
}

} // namespace bit_manipulation::bmd
