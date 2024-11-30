#include <algorithm>
#include <string>
#include <unordered_map>

#include "bmd/directive_type.hpp"
#include "bmd/html/bms_to_html.hpp"
#include "bmd/html/doc_to_html.hpp"
#include "bmd/html/html_writer.hpp"
#include "bmd/parsing/ast.hpp"
#include "bmd/parsing/parse.hpp"

namespace bit_manipulation::bmd {

namespace {

/// @brief The kind of nested language; to be used in `\code` and other directives.
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

    auto it = lookup.find(name);
    if (it == lookup.end()) {
        return {};
    }

    return it->second;
}

/// @brief The architecture; to be used in `\instruction[arch=...]`.
enum struct Architecture { x86, arm, risc_v, power, sparc, mips, ibm_z };

std::optional<Architecture> architecture_by_name(std::string_view name)
{
    if (name.empty()) {
        return {};
    }

    static const std::unordered_map<std::string_view, Architecture> lookup
        = { { "x86", Architecture::x86 },        { "x86_64", Architecture::x86 },
            { "x86-64", Architecture::x86 }, //
            { "arm", Architecture::arm }, //
            { "risc_v", Architecture::risc_v },  { "risc-v", Architecture::risc_v }, //
            { "power", Architecture::power },    { "power-pc", Architecture::power },
            { "power_pc", Architecture::power }, //
            { "sparc", Architecture::sparc },    { "mips", Architecture::mips }, //
            { "ibm_z", Architecture::ibm_z },    { "ibm-z", Architecture::ibm_z } };

    auto it = lookup.find(name);
    if (it == lookup.end()) {
        return {};
    }

    return it->second;
}

std::string_view architecture_link_prefix(Architecture arch)
{
    switch (arch) {
    case Architecture::x86: return "https://www.felixcloutier.com/x86/";
    default: return "";
    }
}

template <typename E>
std::optional<E> attribute_enum_by_name(std::string_view name)
{
    if constexpr (std::is_same_v<E, Architecture>) {
        return architecture_by_name(name);
    }
    else if constexpr (std::is_same_v<E, Nested_Language>) {
        return nested_language_by_name(name);
    }
    else {
        static_assert(dependent_false<E>, "Invalid enum type");
    }
}

template <typename E>
inline constexpr Document_Error_Code enum_document_error_v
    = std::is_same_v<E, Nested_Language> ? Document_Error_Code::invalid_language
                                         : Document_Error_Code::invalid_architecture;

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

    [[nodiscard]] Result<void, Document_Error> convert_content(const ast::List& content)
    {
        static constexpr Tag_Properties paragraph { "p", Formatting_Style::block };

        for (const ast::Some_Node* const p : content.get_children()) {
            if (const auto* const list = get_if<ast::List>(p)) {
                // This would imply an empty paragraph, but it's theoretically impossible to
                // parse one. An empty paragraph would be whitespace, and whitespace should be
                // ignored and dealt with in other places.
                BIT_MANIPULATION_ASSERT(!list->empty());

                m_writer.begin_tag(paragraph);
                if (auto r = convert_list(*list, Formatting_Style::in_line); !r) {
                    return r;
                }
                m_writer.end_tag(paragraph);
            }
            else if (const auto* const directive = get_if<ast::Directive>(p)) {
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
        const std::span<ast::Some_Node* const> children = list.get_children();

        for (Size i = 0; i < children.size(); ++i) {
            const auto* const n = children[i];
            const Local_Source_Span current_pos = get_source_span(*n);

            if (i != 0) {
                if (const auto* const d = get_if<ast::Directive>(children[i - 1])) {
                    m_writer.write_source_gap(d->get_source_position(), current_pos,
                                              Formatting_Style::in_line);
                }
            }

            if (const auto* const text = get_if<ast::Text>(n)) {
                Result<Size, Document_Error> lines_written = convert_text(*text, inherited_style);
                if (!lines_written) {
                    return lines_written.error();
                }
                if (i + 1 >= children.size()) {
                    continue;
                }
                // get is appropriate here because there cannot be two consecutive Text
                // elements
                const auto& d = get<ast::Directive>(*children[i + 1]);

                if (current_pos.end() == d.get_source_position().begin) {
                    continue;
                }
                const bool separate_lines
                    = d.get_source_position().line + 1 > current_pos.line + *lines_written;
                m_writer.write_whitespace(separate_lines ? '\n' : ' ', 1);
                // TODO: this approach isn't perfect because it doesn't handle line breaks
                // after directives properly. It would be best if directives stored what type
                // of whitespace precedes/follows them.

                continue;
            }
            if (const auto* const directive = get_if<ast::Directive>(n)) {
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

    [[nodiscard]] Result<Size, Document_Error> convert_text(const ast::Text& text,
                                                            Formatting_Style inherited_style)
    {
        m_at_start_of_file = false;
        return m_writer.write_inner_text(text.get_text(), inherited_style);
    }

    [[nodiscard]] Result<void, Document_Error> convert_directive(const ast::Directive& directive)
    {
        if (directive_type_is_html_passthrough(directive.get_type())) {
            return convert_html_passthrough_directive(directive);
        }

        switch (directive.get_type()) {
        case Directive_Type::meta: return convert_meta_directive(directive);

        case Directive_Type::code:
        case Directive_Type::code_block: return convert_code_directive(directive);

        case Directive_Type::instruction: return convert_instruction_directive(directive);

        default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Unimplemented directive type.");
        }
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_html_passthrough_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive_type_is_html_passthrough(directive.get_type()));

        m_at_start_of_file = false;

        const Tag_Properties tag { directive_type_tag(directive.get_type()),
                                   directive_type_formatting_style(directive.get_type()) };

        BIT_MANIPULATION_ASSERT(directive.get_block() != nullptr
                                || !directive_type_must_be_empty(directive.get_type()));

        if (directive.get_block() == nullptr && directive.m_arguments.empty()) {
            m_writer.write_empty_tag(tag);
        }
        else {
            begin_tag_with_passed_through_attributes(directive, tag);

            const auto content_type = directive_type_content_type(directive.get_type());
            if (auto r = convert_block(directive.get_block(), tag.style, content_type); !r) {
                return r;
            }
            m_writer.end_tag(tag);
        }
        return {};
    }

    void begin_tag_with_passed_through_attributes(const ast::Directive& directive,
                                                  Tag_Properties properties)
    {
        if (directive.m_arguments.empty()) {
            m_writer.begin_tag(properties);
            return;
        }

        auto attribute_writer = m_writer.begin_tag_with_attributes(properties);
        pass_through_attributes(directive, attribute_writer);
    }

    void pass_through_attributes(const ast::Directive& directive,
                                 Attribute_Writer& attribute_writer)
    {
        for (const auto& [key, value] : directive.m_arguments) {
            if (const auto* const text = get_if<ast::Identifier>(&value)) {
                attribute_writer.write_attribute(key, text->get_value());
                continue;
            }
            if (const auto* const text = get_if<ast::Number>(&value)) {
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
             get<ast::List>(*directive.get_block()).get_children()) {
            const auto& child = get<ast::Directive>(*p);
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
            const Tag_Properties heading { "h1", Formatting_Style::in_line };
            begin_tag_with_passed_through_attributes(*title_directive, heading);
            m_writer.write_inner_text(m_meta.title, heading.style);
            m_writer.end_tag(heading);
        }

        return {};
    }

    [[nodiscard]] Result<void, Document_Error>
    convert_code_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive.get_type() == Directive_Type::code
                                || directive.get_type() == Directive_Type::code_block);

        m_at_start_of_file = false;

        Result<Nested_Language, Document_Error> lang
            = enum_by_attribute(directive.m_arguments, "lang", Nested_Language::bms);
        if (!lang) {
            return lang.error();
        }

        const auto surrounding_tag = directive.get_type() == Directive_Type::code
            ? Tag_Properties { "code", Formatting_Style::in_line }
            : Tag_Properties { "pre", Formatting_Style::block };

        m_writer.begin_tag(surrounding_tag);
        if (auto r = convert_nested_code(directive, *lang); !r) {
            return r;
        }
        m_writer.end_tag(surrounding_tag);

        return {};
    }

    Result<void, Document_Error> convert_nested_code(const ast::Directive& directive,
                                                     Nested_Language lang)
    {
        const auto* const code_text
            = directive.get_block() == nullptr ? nullptr : &get<ast::Text>(*directive.get_block());

        if (lang == Nested_Language::plaintext || code_text == nullptr) {
            if (code_text != nullptr) {
                m_writer.write_inner_text(code_text->get_text(), Formatting_Style::pre);
            }
            return {};
        }

        switch (lang) {
        case Nested_Language::bms: {
            Result<void, Bms_Error> result
                = bms_inline_code_to_html(m_writer, code_text->get_text(), m_memory);
            if (!result && result.error().is_tokenize_error()) {
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

    [[nodiscard]] Result<void, Document_Error>
    convert_instruction_directive(const ast::Directive& directive)
    {
        BIT_MANIPULATION_ASSERT(directive.get_type() == Directive_Type::instruction);

        m_at_start_of_file = false;

        Result<Architecture, Document_Error> arch
            = enum_by_attribute(directive.m_arguments, "arch", Architecture::x86);
        if (!arch) {
            return arch.error();
        }

        auto extension = [&]() -> Result<std::string_view, Document_Error> {
            auto it = directive.m_arguments.find("ext");
            if (it == directive.m_arguments.end()) {
                return std::string_view {};
            }
            const auto* const id = get_if<ast::Identifier>(&it->second);
            if (id == nullptr) {
                // TODO: more accurate diagnostic
                return Document_Error { Document_Error_Code::number_attribute_not_allowed,
                                        directive.get_source_position() };
            }
            return id->get_value();
        }();

        static constexpr Tag_Properties hyperlink { "a", Formatting_Style::in_line };
        static constexpr Tag_Properties code { "code", Formatting_Style::in_line };

        // TODO: deal with potentially empty text (null block)
        const ast::Text& text = get<ast::Text>(*directive.get_block());

        std::string_view link_prefix = architecture_link_prefix(*arch);

        if (!link_prefix.empty()) {
            std::pmr::string link(link_prefix, m_memory);
            link += text.get_text();

            m_writer
                .begin_tag_with_attributes(hyperlink) //
                .write_attribute("href", link)
                .end();
        }
        m_writer.begin_tag(code);
        m_writer.write_inner_text(text.get_text(), Formatting_Style::in_line);
        m_writer.end_tag(code);

        if (!link_prefix.empty()) {
            m_writer.end_tag(hyperlink);
        }

        if (!extension->empty()) {
            constexpr Tag_Properties superscript { "sup", Formatting_Style::in_line };
            m_writer.begin_tag(superscript);
            m_writer.write_inner_text(*extension, Formatting_Style::in_line);
            m_writer.end_tag(superscript);
        }

        return {};
    }

    template <typename E>
    Result<E, Document_Error>
    enum_by_attribute(const ast::Directive::Arguments& attributes, std::string_view key, E fallback)
    {
        const auto lang_pos = attributes.find(key);
        if (lang_pos == attributes.end()) {
            return fallback;
        }
        if (const auto* const lang_number = get_if<ast::Number>(&lang_pos->second)) {
            return Document_Error { Document_Error_Code::number_attribute_not_allowed,
                                    lang_number->get_source_position() };
        }
        if (const auto* const lang_name = get_if<ast::Identifier>(&lang_pos->second)) {
            std::optional<E> result = attribute_enum_by_name<E>(lang_name->get_value());
            if (!result) {
                return Document_Error { enum_document_error_v<E>,
                                        lang_name->get_source_position() };
            }
            return *result;
        }
        // TODO: We could do this more concisely and robustly if we made it possible to
        // access the source position of both Number and Identifier polymorphically.
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unkown kind of value.");
    }

    Result<void, Document_Error> update_metadata_from_directive(const ast::Directive& directive)
    {
        if (directive.get_block() == nullptr) {
            return {};
        }

        switch (directive.get_type()) {
        case Directive_Type::title: {
            const auto& text = get<ast::Text>(*directive.get_block());
            m_meta.title = text.get_text();
            return {};
        }
        case Directive_Type::bms_function: {
            const auto& text = get<ast::Text>(*directive.get_block());
            m_meta.bms_function = text.get_text();
            return {};
        }
        case Directive_Type::c_equivalent: {
            const auto& text = get<ast::Text>(*directive.get_block());
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
            if (auto r = convert_text(get<ast::Text>(*block), inherited_style); !r) {
                return r.error();
            }
            return {};

        case Directive_Content_Type::span:
        case Directive_Content_Type::meta:
        case Directive_Content_Type::list:
            return convert_list(get<ast::List>(*block), inherited_style);

        case Directive_Content_Type::block: return convert_content(get<ast::List>(*block));
        }

        BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid contents for Block.");
    }
};

} // namespace

Result<void, Document_Error> doc_to_html(HTML_Token_Consumer& out,
                                         const Parsed_Document& document,
                                         Document_Options options,
                                         std::pmr::memory_resource* memory)
{
    static constexpr Tag_Properties html { "html", Formatting_Style::flat };
    static constexpr Tag_Properties body { "body", Formatting_Style::flat };
    static constexpr Tag_Properties head { "head", Formatting_Style::block };
    static constexpr Tag_Properties link { "link", Formatting_Style::block };

    HTML_Writer writer { out, options.indent_width };
    writer.write_preamble();

    writer.begin_tag(html);

    writer.begin_tag(head);
    for (const std::string_view sheet : options.stylesheets) {
        writer.begin_tag_with_attributes(link)
            .write_attribute("rel", "stylesheet")
            .write_attribute("href", sheet)
            .end_empty();
    }
    writer.end_tag(head);

    writer.begin_tag(body);

    if (document.root_node != nullptr) {
        const auto& root_content = get<ast::List>(*document.root_node);
        auto r = HTML_Converter { writer, document, memory }.convert_content(root_content);
        if (!r) {
            return r;
        }
    }

    writer.end_tag(body);
    writer.end_tag(html);
    if (!writer.is_done()) {
        return Document_Error { Document_Error_Code::writer_misuse, {} };
    }
    return {};
}

} // namespace bit_manipulation::bmd
