#include <algorithm>

#include "bmd/directive_type.hpp"
#include "bmd/html_writer.hpp"

namespace bit_manipulation::bmd {

bool directive_content_allows(Directive_Content_Type content, Directive_Environment environment)
{
    using enum Directive_Content_Type;
    switch (content) {
    case nothing: return false;
    case text_span: return false;
    case span: return environment == Directive_Environment::paragraph;
    case block:
        return environment == Directive_Environment::paragraph
            || environment == Directive_Environment::content;
    case meta: return environment == Directive_Environment::meta;
    case list: return environment == Directive_Environment::list;
    case raw: return true;
    }

    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid content type.");
}

Formatting_Style directive_type_formatting_style(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case bold:
    case heading1:
    case heading2:
    case heading3:
    case heading4:
    case heading5:
    case heading6:
    case line_break:
    case deleted:
    case instruction:
    case italic:
    case item:
    case keyboard:
    case quoted:
    case strikethrough:
    case subscript:
    case superscript:
    case teletype:
    case underlined:
    case code: return Formatting_Style::in_line;

    case code_block:
    case ordered_list:
    case note:
    case unordered_list: return Formatting_Style::block;

    case horizontal_rule: return Formatting_Style::flat;

    case meta:
    case title:
    case bms_function:
    case c_equivalent: return {};
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid directive type.");
}

Directive_Content_Type directive_type_content_type(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case line_break:
    case horizontal_rule: return Directive_Content_Type::nothing;

    case bms_function:
    case c_equivalent:
    case title: return Directive_Content_Type::text_span;

    case bold:
    case code:
    case deleted:
    case heading1:
    case heading2:
    case heading3:
    case heading4:
    case heading5:
    case heading6:
    case instruction:
    case italic:
    case item:
    case keyboard:
    case quoted:
    case strikethrough:
    case subscript:
    case superscript:
    case teletype:
    case underlined: return Directive_Content_Type::span;

    case code_block: return Directive_Content_Type::raw;

    case note: return Directive_Content_Type::block;

    case ordered_list:
    case unordered_list: return Directive_Content_Type::list;

    case meta: return Directive_Content_Type::meta;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid directive type.");
}

Directive_Environment directive_type_environment(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {

    case bold:
    case line_break:
    case deleted:
    case instruction:
    case italic:
    case keyboard:
    case quoted:
    case strikethrough:
    case subscript:
    case superscript:
    case teletype:
    case underlined:
    case code: return Directive_Environment::paragraph;

    case heading1:
    case heading2:
    case heading3:
    case heading4:
    case heading5:
    case heading6:
    case code_block:
    case ordered_list:
    case meta:
    case note:
    case unordered_list:
    case horizontal_rule: return Directive_Environment::content;

    case title:
    case bms_function:
    case c_equivalent: return Directive_Environment::meta;

    case item: return Directive_Environment::list;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid directive type.");
}

std::optional<Directive_Type> directive_type_by_id(std::string_view directive_id) noexcept
{
    using enum Directive_Type;

    static constexpr struct Pair {
        std::string_view id;
        Directive_Type type;
    } lookup[] {
        { "b", bold },
        { "bmsfun", bms_function },
        { "br", line_break },
        { "c", code },
        { "cequiv", c_equivalent },
        { "codeblock", code_block },
        { "del", deleted },
        { "h1", heading1 },
        { "h2", heading2 },
        { "h3", heading3 },
        { "h4", heading4 },
        { "h5", heading5 },
        { "h6", heading6 },
        { "hr", horizontal_rule },
        { "i", italic },
        { "instruction", instruction },
        { "item", item },
        { "kbd", keyboard },
        { "meta", meta },
        { "note", note },
        { "ol", ordered_list },
        { "q", quoted },
        { "s", strikethrough },
        { "sub", subscript },
        { "sup", superscript },
        { "title", title },
        { "tt", teletype },
        { "u", underlined },
        { "ul", unordered_list },
    };

    static_assert(std::ranges::is_sorted(lookup, {}, &Pair::id));

    const auto it = std::ranges::lower_bound(lookup, directive_id, {}, &Pair::id);
    if (it->id != directive_id) {
        return {};
    }
    return it->type;
}

std::string_view directive_type_tag(Directive_Type type)
{
    using enum Directive_Type;
    switch (type) {
    case bold: return "b";
    case line_break: return "br";
    case deleted: return "del";
    case heading1: return "h1";
    case heading2: return "h2";
    case heading3: return "h3";
    case heading4: return "h4";
    case heading5: return "h5";
    case heading6: return "h6";
    case horizontal_rule: return "hr";
    case italic: return "i";
    case keyboard: return "kbd";
    case ordered_list: return "ol";
    case quoted: return "q";
    case strikethrough: return "s";
    case subscript: return "sub";
    case superscript: return "sub";
    case teletype: return "tt";
    case underlined: return "u";
    case unordered_list: return "ul";
    case code: return "code";
    case code_block: return "div";
    case instruction: return "span";
    case note: return "div";
    case meta: return "head";
    case title: return "title";

    case bms_function:
    case c_equivalent:
    case item: return "";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid directive type.");
}

} // namespace bit_manipulation::bmd
