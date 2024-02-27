#include <algorithm>
#include <unordered_map>

#include "bmd/bms_to_html.hpp"
#include "bmd/directive_type.hpp"
#include "bmd/doc_to_html.hpp"
#include "bmd/html_writer.hpp"
#include "bmd/parse.hpp"

namespace bit_manipulation::bmd {

namespace {

enum struct Nested_Language {
    plaintext,
    bms,
    bmd,
    c,
    cxx,
    rust,
};

std::optional<Nested_Language> nested_language_by_name(std::string_view name)
{
    if (name.empty()) {
        return {};
    }

    static const std::unordered_map<std::string_view, Nested_Language> lookup
        = { //
            { "bmd", Nested_Language::bmd },
            { "bms", Nested_Language::bms },
            { "c", Nested_Language::c },
            { "c++", Nested_Language::cxx },
            { "cxx", Nested_Language::cxx },
            { "cpp", Nested_Language::cxx },
            { "plain", Nested_Language::plaintext },
            { "plaintext", Nested_Language::plaintext },
            { "text", Nested_Language::plaintext },
            { "raw", Nested_Language::plaintext },
            { "rust", Nested_Language::rust }
          };

    if (name == "c++" || name == "cpp" || name == "cxx") {
        return Nested_Language::cxx;
    }
    if (name == "plain" || name == "none" || name == "plaintext" || name == "txt"
        || name == "text") {
        return Nested_Language::plaintext;
    }
    if (name == "rust") {
        return Nested_Language::rust;
    }
    if (name == "bms") {
        return Nested_Language::bms;
    }
    if (name == "bmd") {
        return Nested_Language::bmd;
    }
    return {};
}

struct HTML_Converter {
    HTML_Writer& m_writer;
    const Parsed_Document& m_document;
    std::pmr::memory_resource* m_memory;

    bool m_at_start_of_file = true;

    struct Metadata {
        std::string_view title;
        std::string_view bms_function;
        std::string_view c_equivalent;
    } m_meta;

    explicit HTML_Converter(HTML_Writer& writer,
                            const Parsed_Document& document,
                            std::pmr::memory_resource* memory)
        : m_writer(writer)
        , m_document(document)
        , m_memory(memory)
    {
    }

    [[nodiscard]] Result<void, Document_Error> convert_content(const ast::List& content,
                                                               Formatting_Style inherited_style)
    {
        for (const ast::Some_Node* const p : content.get_children()) {
            if (const auto* const list = std::get_if<ast::List>(p)) {
                // This would imply an empty paragraph, but it's theoretically impossible to
                // parse one. An empty paragraph would be whitespace, and whitespace should be
                // ignored and dealt with in other places.
                BIT_MANIPULATION_ASSERT(!list->empty());

                m_writer.begin_tag("p", Formatting_Style::block);
                if (auto r = convert_list(*list, inherited_style); !r) {
                    return r;
                }
                m_writer.end_tag("p", Formatting_Style::block);
            }
            else if (const auto* const directive = std::get_if<ast::Directive>(p)) {
                // Directives which aren't part of a paragraph but go directly into the content
                // should not appear at a top-level here.
                // Even if there was say, a paragraph that contains a single `\b{Hi!}`, this
                // should be wrapped in a paragraph which contains the directive.
                // Directives which can only appear in directive lists, such as `\item` would
                // also be out of place here.
                BIT_MANIPULATION_ASSERT(directive_type_environment(directive->get_type())
                                        == Directive_Environment::content);

                if (auto r = convert_directive(*directive); !r) {
                    return r;
                }
            }
            else {
                BIT_MANIPULATION_ASSERT_UNREACHABLE(
                    "Content can only contain lists or directives.");
            }
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> convert_list(const ast::List& list,
                                                            Formatting_Style inherited_style)
    {
        for (const ast::Some_Node* const n : list.get_children()) {
            if (const auto* const text = std::get_if<ast::Text>(n)) {
                if (auto r = convert_text(*text, inherited_style); !r) {
                    return r;
                }
                continue;
            }
            if (const auto* const directive = std::get_if<ast::Directive>(n)) {
                // Directives in a content environment should only be processed by
                // `convert_content` directly.
                // Finding a directive with this environment suggests that either we have
                // misused `convert_list`, or the AST has structural problems.
                BIT_MANIPULATION_ASSERT(directive_type_environment(directive->get_type())
                                        != Directive_Environment::content);

                if (auto r = convert_directive(*directive); !r) {
                    return r;
                }
                continue;
            }
            // Lists containing lists are possible, but handled in `convert_content`, which is
            // the only place where this can occur.
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Lists should only contain text or directives.");
        }
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> convert_text(const ast::Text& text,
                                                            Formatting_Style inherited_style)
    {
        m_at_start_of_file = false;
        m_writer.write_inner_text(text.get_text(), inherited_style);
        return {};
    }

    [[nodiscard]] Result<void, Document_Error> convert_directive(const ast::Directive& directive)
    {
        if (directive_type_is_html_passthrough(directive.get_type())) {
            return convert_html_passthrough_directive(directive);
        }

        switch (directive.get_type()) {
        case Directive_Type::meta: return convert_meta_directive(directive);
        case Directive_Type::code: return convert_code_directive(directive);
        default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unimplemented directive type.");
        }
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_html_passthrough_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive_type_is_html_passthrough(directive.get_type()));

        m_at_start_of_file = false;

        const std::string_view tag = directive_type_tag(directive.get_type());
        const Formatting_Style style = directive_type_formatting_style(directive.get_type());

        BIT_MANIPULATION_ASSERT(directive.get_block() != nullptr
                                || !directive_type_must_be_empty(directive.get_type()));

        if (directive.get_block() == nullptr && directive.m_arguments.empty()) {
            m_writer.write_empty_tag(tag, style);
        }
        else {
            begin_tag_with_passed_through_attributes(directive, tag, style);

            const auto content_type = directive_type_content_type(directive.get_type());
            if (auto r = convert_block(directive.get_block(), style, content_type); !r) {
                return r;
            }
            m_writer.end_tag(tag, style);
        }
        return {};
    }

    void begin_tag_with_passed_through_attributes(const ast::Directive& directive,
                                                  std::string_view tag,
                                                  Formatting_Style style)
    {
        if (directive.m_arguments.empty()) {
            m_writer.begin_tag(tag, style);
            return;
        }

        auto attribute_writer = m_writer.begin_tag_with_attributes(tag, style);
        pass_through_attributes(directive, attribute_writer);
    }

    void pass_through_attributes(const ast::Directive& directive,
                                 Attribute_Writer& attribute_writer)
    {
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

    [[nodiscard]] Result<void, Document_Error>
    convert_meta_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive.get_type() == Directive_Type::meta);

        if (!m_at_start_of_file) {
            return Document_Error { Document_Error_Code::meta_not_at_start_of_file,
                                    directive.get_source_position() };
        }
        m_at_start_of_file = false;

        if (directive.get_block() == nullptr) {
            return {};
        }

        const ast::Directive* title_directive = nullptr;
        for (const ast::Some_Node* const p :
             std::get<ast::List>(*directive.get_block()).get_children()) {
            const auto& child = std::get<ast::Directive>(*p);
            if (auto r = update_metadata_from_directive(child); !r) {
                return r;
            }
            if (child.get_type() == Directive_Type::title) {
                if (title_directive != nullptr) {
                    // TODO: check this for other entries as well
                    return Document_Error { Document_Error_Code::duplicate_meta_entry,
                                            child.get_source_position() };
                }
                title_directive = &child;
            }
        }

        if (title_directive != nullptr) {
            constexpr auto style = Formatting_Style::in_line;
            begin_tag_with_passed_through_attributes(*title_directive, "h1", style);
            m_writer.write_inner_text(m_meta.title, style);
            m_writer.end_tag("h1", style);
        }

        return {};
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_code_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive.get_type() == Directive_Type::code);

        if (!m_at_start_of_file) {
            return Document_Error { Document_Error_Code::meta_not_at_start_of_file,
                                    directive.get_source_position() };
        }
        m_at_start_of_file = false;

        const auto lang_pos = directive.m_arguments.find("lang");
        const auto lang = [&]() -> Result<Nested_Language, Document_Error> {
            if (lang_pos == directive.m_arguments.end()) {
                return Nested_Language::bms;
            }
            if (const auto* const lang_number = std::get_if<ast::Number>(&lang_pos->second)) {
                return Document_Error { Document_Error_Code::number_attribute_not_allowed,
                                        lang_number->get_source_position() };
            }
            if (const auto* const lang_name = std::get_if<ast::Identifier>(&lang_pos->second)) {
                std::optional<Nested_Language> lang_by_name
                    = nested_language_by_name(lang_name->get_value());
                if (!lang_by_name) {
                    return Document_Error { Document_Error_Code::invalid_language,
                                            lang_name->get_source_position() };
                }
                return *lang_by_name;
            }
            // TODO: We could do this more concisely and robustly if we made it possible to
            // access the source position of both Number and Identifier polymorphically.
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Unkown kind of value.");
        }();
        if (!lang) {
            return lang.error();
        }

        const auto* const code_text = directive.get_block() == nullptr
            ? nullptr
            : &std::get<ast::Text>(*directive.get_block());

        if (*lang == Nested_Language::plaintext || code_text == nullptr) {
            m_writer.begin_tag("code", Formatting_Style::in_line);
            if (code_text != nullptr) {
                m_writer.write_inner_text(code_text->get_text(), Formatting_Style::pre);
            }
            m_writer.end_tag("code", Formatting_Style::in_line);
            return {};
        }

        BIT_MANIPULATION_ASSERT(code_text != nullptr);

        switch (*lang) {
        case Nested_Language::bms: {
            Result<void, Bms_Error> result
                = bms_inline_code_to_html(m_writer, code_text->get_text(), m_memory);
            if (!result && std::holds_alternative<bms::Tokenize_Error>(result.error())) {
                return Document_Error { Document_Error_Code::code_tokenization_failure,
                                        code_text->get_source_position() };
            }
            break;
        }
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE(
                "Only bms and plaintext highlighting is implemented.");
        }

        return {};
    }

    Result<void, Document_Error> update_metadata_from_directive(const ast::Directive& directive)
    {
        if (directive.get_block() == nullptr) {
            return {};
        }

        switch (directive.get_type()) {
        case Directive_Type::title: {
            const auto& text = std::get<ast::Text>(*directive.get_block());
            m_meta.title = text.get_text();
            return {};
        }
        case Directive_Type::bms_function: {
            const auto& text = std::get<ast::Text>(*directive.get_block());
            m_meta.bms_function = text.get_text();
            return {};
        }
        case Directive_Type::c_equivalent: {
            const auto& text = std::get<ast::Text>(*directive.get_block());
            m_meta.c_equivalent = text.get_text();
            return {};
        }

        default:
            return Document_Error { Document_Error_Code::directive_not_allowed,
                                    directive.get_source_position() };
        }
    }

    Result<void, Document_Error> convert_block(const ast::Some_Node* block,
                                               Formatting_Style inherited_style,
                                               Directive_Content_Type context)
    {
        if (block == nullptr) {
            return {};
        }
        switch (context) {
        case Directive_Content_Type::nothing:
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Non-empty block is not allowed.");

        case Directive_Content_Type::raw:
        case Directive_Content_Type::text_span:
            return convert_text(std::get<ast::Text>(*block), inherited_style);

        case Directive_Content_Type::span:
        case Directive_Content_Type::meta:
        case Directive_Content_Type::list:
            return convert_list(std::get<ast::List>(*block), inherited_style);

        case Directive_Content_Type::block:
            return convert_content(std::get<ast::List>(*block), inherited_style);
        }

        BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid contents for Block.");
    }
};

} // namespace

Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out,
                                         const Parsed_Document& document,
                                         Size indent_width,
                                         std::pmr::memory_resource* memory)
{
    HTML_Writer writer { out, indent_width };
    writer.write_preamble();

    writer.begin_tag("html", Formatting_Style::flat);
    writer.begin_tag("body", Formatting_Style::flat);

    if (document.root_node != nullptr) {
        const auto& root_content = std::get<ast::List>(*document.root_node);
        auto r = HTML_Converter { writer, document, memory }.convert_content(
            root_content, Formatting_Style::flat);
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
