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

#if 0
    struct {
        std::string_view id;
        std::string_view tag;
    } tags_by_id[] = {
        { "i", "i" }, //
        { "b", "b" }, //
        { "u", "u" }, //
        { "s", "s" }, //
        { "q", "q" },
    };
#endif

    return {};
}

struct HTML_Converter {
    HTML_Writer& writer;

    void operator()(const ast::Content& content, Formatting_Style inherited_style)
    {
        for (const ast::Some_Node* const p : content.get_children()) {
            operator()(std::get<ast::Paragraph>(*p), inherited_style);
        }
    }

    void operator()(const ast::Paragraph& paragraph, Formatting_Style)
    {
        writer.begin_tag("p", Formatting_Style::block);
        for (const ast::Some_Node* const n : paragraph.get_children()) {
            if (const auto* const text = std::get_if<ast::Text>(n)) {
                operator()(*text, Formatting_Style::block);
                continue;
            }
            if (const auto* const directive = std::get_if<ast::Directive>(n)) {
                operator()(*directive, Formatting_Style::block);
                continue;
            }
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Paragraphs should only contain text or directives.");
        }
        writer.end_tag("p", Formatting_Style::block);
    }

    void operator()(const ast::Directive& directive, Formatting_Style)
    {
        const Tag_Info info = tag_of(directive);

        if (!directive.m_arguments.empty()) {
            BIT_MANIPULATION_ASSERT(info.is_passthrough); // otherwise not implemented yet
            auto attribute_writer = writer.begin_tag_with_attributes(info.tag, info.style);
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
            writer.write_empty_tag(info.tag, info.style);
            return;
        }
        writer.begin_tag(info.tag, info.style);
        operator()(directive.get_block(), info.style);
        writer.end_tag(info.tag, info.style);
    }

    void operator()(const ast::Text& text, Formatting_Style inherited_style)
    {
        writer.write_inner_text(text.get_text(), inherited_style);
    }

    void operator()(const ast::Some_Node* node, Formatting_Style inherited_style)
    {
        BIT_MANIPULATION_ASSERT(node != nullptr);
        return fast_visit(
            [this, inherited_style](const auto& n) { return operator()(n, inherited_style); },
            *node);
    }
};

} // namespace

void doc_to_html(HTML_Token_Consumer& out, const Parsed_Document& document)
{
    HTML_Writer writer { out };
    writer.write_preamble();

    writer.begin_tag("html", Formatting_Style::flat);
    writer.begin_tag("body", Formatting_Style::flat);

    if (document.root_node != nullptr) {
        HTML_Converter { writer }(document.root_node, Formatting_Style::flat);
    }

    writer.end_tag("body", Formatting_Style::flat);
    writer.end_tag("html", Formatting_Style::flat);
}

} // namespace bit_manipulation::bmd
