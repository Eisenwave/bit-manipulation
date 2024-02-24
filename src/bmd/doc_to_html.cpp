#include <algorithm>

#include "bmd/directive_type.hpp"
#include "bmd/doc_to_html.hpp"
#include "bmd/html_writer.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation::bmd {

namespace {

struct HTML_Converter {
    enum struct Context {
        /// The default context.
        normal,
        /// Inside a \meta directive.
        meta,
    };

    HTML_Writer& m_writer;
    bool m_at_start_of_file = true;

    explicit HTML_Converter(HTML_Writer& writer)
        : m_writer { writer }
    {
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_content(const ast::Content& content, Formatting_Style inherited_style, Context context)
    {
        for (const ast::Some_Node* const p : content.get_children()) {
            auto r = convert_paragraph(std::get<ast::Paragraph>(*p), inherited_style, context);
            if (!r) {
                return r;
            }
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> convert_paragraph(const ast::Paragraph& paragraph,
                                                                 Formatting_Style inherited_style,
                                                                 Context context)
    {
        m_writer.begin_tag("p", Formatting_Style::block);
        for (const ast::Some_Node* const n : paragraph.get_children()) {
            if (const auto* const text = std::get_if<ast::Text>(n)) {
                if (auto r = convert_text(*text, inherited_style, context); !r) {
                    return r;
                }
                continue;
            }
            if (const auto* const directive = std::get_if<ast::Directive>(n)) {
                if (auto r = convert_directive(*directive, inherited_style, context); !r) {
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

    [[nodiscard]] Result<void, Document_Error>
    convert_text(const ast::Text& text, Formatting_Style inherited_style, Context context)
    {
        if (context == Context::meta) {
            return Document_Error { Document_Error_Code::text_not_allowed,
                                    text.get_source_position() };
        }

        m_at_start_of_file = false;
        m_writer.write_inner_text(text.get_text(), inherited_style);
        return {};
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_directive(const ast::Directive& directive, Formatting_Style, Context context)
    {
        std::optional<Directive_Type> type = directive_type_by_id(directive.get_identifier());
        if (!type) {
            return Document_Error { Document_Error_Code::unknown_directive,
                                    directive.get_source_position() };
        }
        if (directive_type_is_html_passthrough(*type)) {
            return convert_html_passthrough_directive(directive, *type);
        }

        switch (*type) {
        case Directive_Type::meta: return convert_meta_directive(directive);
        }

        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unimplemented directive type.");

        return {};
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_html_passthrough_directive(const ast::Directive& directive, Directive_Type type)
    {
        BIT_MANIPULATION_ASSERT(directive_type_is_html_passthrough(type));

        m_at_start_of_file = false;

        const std::string_view tag = directive_type_tag(type);
        const Formatting_Style style = directive_type_formatting_style(type);

        if (!directive.m_arguments.empty()) {
            auto attribute_writer = m_writer.begin_tag_with_attributes(tag, style);
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
            if (directive_type_must_be_empty(type)) {
                return Document_Error { Document_Error_Code::no_block_allowed,
                                        directive.get_source_position() };
            }
            m_writer.write_empty_tag(tag, style);
        }
        else {
            m_writer.begin_tag(tag, style);
            if (auto r = convert_block(directive.get_block(), style, Context::normal); !r) {
                return r;
            }
            m_writer.end_tag(tag, style);
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_meta_directive(const ast::Directive& directive)
    {
        if (!m_at_start_of_file) {
            return Document_Error { Document_Error_Code::meta_not_at_start_of_file,
                                    directive.get_source_position() };
        }
        m_at_start_of_file = false;
        return {};
    }

    Result<void, Document_Error>
    convert_block(const ast::Some_Node* block, Formatting_Style inherited_style, Context context)
    {
        if (block == nullptr) {
            return {};
        }
        if (const auto* const content = std::get_if<ast::Content>(block)) {
            return convert_content(*content, inherited_style, context);
        }
        if (const auto* const text = std::get_if<ast::Text>(block)) {
            return convert_text(*text, inherited_style, context);
        }

        BIT_MANIPULATION_ASSERT_UNREACHABLE(
            "Blocks can only contain content or raw content (text).");
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
        const auto& root_content = std::get<ast::Content>(*document.root_node);
        auto r = HTML_Converter { writer }.convert_content(root_content, Formatting_Style::flat,
                                                           HTML_Converter::Context::normal);
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
