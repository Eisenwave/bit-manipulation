#ifndef BIT_MANIPULATION_BMS_AST_HPP
#define BIT_MANIPULATION_BMS_AST_HPP

#include <optional>
#include <span>
#include <vector>

#include "common/tribool.hpp"
#include "common/variant.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analysis_level.hpp"
#include "bms/annotation.hpp"
#include "bms/concrete_type.hpp"
#include "bms/debug_info.hpp"
#include "bms/deduction.hpp"
#include "bms/expression_type.hpp"
#include "bms/fwd.hpp"
#include "bms/lookup_result.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms {

struct Parameter {
private:
    std::string_view m_name;
    std::optional<Source_Span> m_position;
    ast::Some_Node* m_type;

public:
    Parameter(const astp::Parameter& parsed, std::string_view file);

    std::string_view get_name() const
    {
        return m_name;
    }

    std::optional<Source_Span> get_position() const
    {
        return m_position;
    }

    ast::Some_Node* get_type_node()
    {
        return m_type;
    }
    const ast::Some_Node* get_type_node() const
    {
        return m_type;
    }
    void set_type_node(ast::Some_Node* node)
    {
        m_type = node;
    }

    ast::Type& get_type();
    const ast::Type& get_type() const;

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::parameter, get_position(), get_name() };
    }
};

namespace ast {
namespace detail {

struct Annotations {
private:
    std::span<const astp::Handle> m_parsed;
    std::pmr::vector<Annotation> m_concrete {};

public:
    [[nodiscard]] explicit Annotations(std::span<const astp::Handle> parsed)
        : m_parsed(parsed)
    {
    }

    [[nodiscard]] bool were_analyzed() const
    {
        return m_parsed.size() == m_concrete.size();
    }

    friend Resolve_Annotations;
};

struct Node_Base {
public:
    [[nodiscard]] static Node_Base make_builtin() noexcept
    {
        return Node_Base {};
    }

protected:
    struct Root_Node_Tag { };

    Some_Node* m_parent;
    std::optional<Source_Span> m_position;
    /// @brief If present, indicates that type analysis for an AST node is complete.
    /// For some nodes such as expression nodes, this also indicates the value of the node, if it
    /// is a constant.
    std::optional<Value> m_const_value;

    explicit Node_Base();

    explicit Node_Base(Root_Node_Tag, const astp::detail::Node_Base& parsed, std::string_view file);

    explicit Node_Base(Some_Node& parent,
                       const astp::detail::Node_Base& parsed,
                       std::string_view file);

    explicit Node_Base(Some_Node& parent, const Source_Span& pos, std::optional<Value> value = {});

public:
    std::optional<Source_Span> get_position() const
    {
        return m_position;
    }

    std::optional<Value>& const_value()
    {
        return m_const_value;
    }

    const std::optional<Value>& const_value() const
    {
        return m_const_value;
    }

    /// @brief Returns `true` if semantic analysis was completed for this node.
    bool was_analyzed() const
    {
        return m_const_value.has_value();
    }

    Some_Node* get_parent()
    {
        return m_parent;
    }

    const Some_Node* get_parent() const
    {
        return m_parent;
    }

    void set_parent(Some_Node& parent)
    {
        BIT_MANIPULATION_ASSERT(m_parent != nullptr);
        m_parent = &parent;
    }
};

template <int N>
struct Parent {
protected:
    Some_Node* m_children[N] {};

public:
    std::span<Some_Node*> get_children();

    std::span<const Some_Node* const> get_children() const;

    template <std::forward_iterator Forward_It, std::forward_iterator Sentinel>
    void set_children(Forward_It begin, Sentinel end);

    template <std::ranges::forward_range R>
    void set_children(R&& r)
    {
        set_children(std::ranges::begin(r), std::ranges::end(r));
    }
};

template <>
struct Parent<0> {

    std::span<Some_Node*> get_children()
    {
        return {};
    }

    std::span<const Some_Node* const> get_children() const
    {
        return {};
    }

    template <std::forward_iterator Forward_It, std::forward_iterator Sentinel>
    void set_children(Forward_It begin, Sentinel end)
    {
        BIT_MANIPULATION_ASSERT(begin == end);
    }

    template <std::ranges::forward_range R>
    void set_children(R&& r)
    {
        set_children(std::ranges::begin(r), std::ranges::end(r));
    }
};

struct Dynamic_Parent {
protected:
    std::pmr::vector<Some_Node*> m_children;

public:
    explicit Dynamic_Parent(std::pmr::memory_resource* memory)
        : m_children(memory)
    {
    }

    std::span<Some_Node*> get_children();

    std::span<const Some_Node* const> get_children() const;

    template <std::ranges::forward_range R>
    void set_children(R&& r)
    {
        m_children.clear();
        m_children.insert(m_children.begin(), r.begin(), r.end());
    }
};

struct Annotated {
protected:
    detail::Annotations m_annotations;

    explicit Annotated(detail::Annotations&& annotations)
        : m_annotations { std::move(annotations) }
    {
    }

    friend Resolve_Annotations;
};

} // namespace detail

struct Program final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Program";
    static inline constexpr bool is_expression = false;

    Program(const astp::Program& parsed, std::string_view file, std::pmr::memory_resource* memory);

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::program, get_position() };
    }
};

struct Function final : detail::Node_Base, detail::Annotated {
private:
    struct Copy_for_Instantiation_Tag { };

public:
    static inline constexpr std::string_view self_name = "Function";
    static inline constexpr std::string_view child_names[]
        = { "parameters", "return_type", "requires_clause", "body" };
    static inline constexpr bool is_expression = false;

    struct Instance {
    private:
        std::pmr::vector<int> widths;
        Some_Node* handle;

    public:
        [[nodiscard]] Instance(std::pmr::vector<int>&& widths, Some_Node* handle);

        [[nodiscard]] Some_Node* get_function_node()
        {
            return handle;
        }

        [[nodiscard]] const Some_Node* get_function_node() const
        {
            return handle;
        }

        [[nodiscard]] Function& get_function();

        [[nodiscard]] const Function& get_function() const;

        [[nodiscard]] bool has_widths(const Widths& w) const noexcept
        {
            for (Size i = 0; i < widths.size(); ++i) {
                if (widths[i] != get_width(w, i)) {
                    return false;
                }
            }
            return true;
        }
    };

private:
    static inline constexpr Size invalid_vm_address = Size(-1);

    std::string_view m_name;
    std::pmr::vector<Parameter> m_parameters;
    Some_Node* m_return_type = nullptr;
    Some_Node* m_requires_clause = nullptr;
    Some_Node* m_body = nullptr;
    Size m_vm_address = invalid_vm_address;

public:
    std::pmr::vector<Instance> instances;
    bool is_generic = false;
    Analysis_Level analysis_so_far = Analysis_Level::unanalyzed;
    Tribool definitely_returns = Tribool::maybe;

private:
    Function(const Function& other, Copy_for_Instantiation_Tag);

public:
    Function(Some_Node& parent,
             const astp::Function& parsed,
             std::string_view file,
             std::pmr::memory_resource* memory,
             std::span<const astp::Handle> annotations);

    [[nodiscard]] std::string_view get_name() const
    {
        return m_name;
    }

    [[nodiscard]] std::optional<Size> get_vm_address() const
    {
        if (m_vm_address == invalid_vm_address) {
            return {};
        }
        return m_vm_address;
    }

    void set_vm_address(Size address)
    {
        m_vm_address = address;
    }

    [[nodiscard]] Size get_parameter_count() const
    {
        return get_parameters().size();
    }

    std::pmr::vector<Parameter>& get_parameters()
    {
        return m_parameters;
    }
    std::span<const Parameter> get_parameters() const
    {
        return m_parameters;
    }

    Some_Node* get_return_type_node()
    {
        return m_return_type;
    }
    const Some_Node* get_return_type_node() const
    {
        return m_return_type;
    }
    void set_return_type_node(Some_Node* node)
    {
        m_return_type = node;
    }

    Type& get_return_type();
    const Type& get_return_type() const;

    Some_Node* get_requires_clause_node()
    {
        return m_requires_clause;
    }
    const Some_Node* get_requires_clause_node() const
    {
        return m_requires_clause;
    }
    void set_requires_clause_node(Some_Node* node)
    {
        m_requires_clause = node;
    }

    Some_Node* get_body_node()
    {
        return m_body;
    }
    const Some_Node* get_body_node() const
    {
        return m_body;
    }
    void set_body_node(Some_Node* node)
    {
        m_body = node;
    }

    Block_Statement& get_body();
    const Block_Statement& get_body() const;

    Function copy_for_instantiation() const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        return { *this, Copy_for_Instantiation_Tag {} };
    }

    [[nodiscard]] const Instance* find_instance(const Widths& w) const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        for (const Instance& instance : instances) {
            if (instance.has_widths(w)) {
                return &instance;
            }
        }
        return nullptr;
    }
    [[nodiscard]] Instance* find_instance(const Widths& w)
    {
        return const_cast<Instance*>(std::as_const(*this).find_instance(w));
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::function, get_position(), get_name() };
    }

    /// @brief Returns debug info for the return type.
    /// This function is safe to call even if there is no declared return type,
    /// in which case info with `Construct::implicit_type` is returned.
    [[nodiscard]] Debug_Info get_return_type_debug_info() const;

    /// @brief Returns the return type of this function as a `Concrete_Type`.
    /// The declared return type (if any) shall have been analyzed,
    /// and this function shall not be generic.
    /// If no return type was declared, `Concrete_Type::Void` is returned.
    [[nodiscard]] Concrete_Type get_concrete_return_type() const;
};

struct Type final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Type";
    static inline constexpr std::string_view child_names[] = { "width" };
    static inline constexpr bool is_expression = false;

private:
    Type_Type m_type;

public:
    std::optional<int> concrete_width;

    [[nodiscard]] Type(Some_Node& parent, const astp::Type& parsed, std::string_view file);

    [[nodiscard]] Type_Type get_type() const
    {
        return m_type;
    }

    [[nodiscard]] Some_Node* get_width_node()
    {
        return m_children[0];
    }
    [[nodiscard]] const Some_Node* get_width_node() const
    {
        return m_children[0];
    }

    /// @brief Returns a concrete type based on the information in this node, or `std::nullopt`
    /// if the information is incomplete.
    /// Namely, if the type is `Uint`, a concrete width must be known, otherwise this function
    /// always succeeds.
    /// @return A concrete type, or `std::nullopt`.
    [[nodiscard]] std::optional<Concrete_Type> concrete_type() const
    {
        if (m_type == Type_Type::Uint) {
            if (concrete_width) {
                return Concrete_Type::Uint(*concrete_width);
            }
            return {};
        }
        // For anything but UInt, why would concrete_width be set?
        BIT_MANIPULATION_ASSERT(!concrete_width);
        return Concrete_Type { m_type };
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::type, get_position() };
    }
};

struct Const final : detail::Node_Base, detail::Annotated, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Const";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };
    static inline constexpr bool is_expression = false;

private:
    std::string_view m_name;

public:
    Const(Some_Node& parent,
          const astp::Const& parsed,
          std::string_view file,
          std::span<const astp::Handle> annotations);

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_type_node()
    {
        return m_children[0];
    }
    const Some_Node* get_type_node() const
    {
        return m_children[0];
    }

    Type& get_type();
    const Type& get_type() const;

    Some_Node* get_initializer_node()
    {
        return m_children[1];
    }
    const Some_Node* get_initializer_node() const
    {
        return m_children[1];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::constant, get_position(), get_name() };
    }
};

struct Let final : detail::Node_Base, detail::Annotated, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Let";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };
    static inline constexpr bool is_expression = false;

private:
    std::string_view m_name;

public:
    Let(Some_Node& parent,
        const astp::Let& parsed,
        std::string_view file,
        std::span<const astp::Handle> annotations);

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_type_node()
    {
        return m_children[0];
    }
    const Some_Node* get_type_node() const
    {
        return m_children[0];
    }

    Type& get_type();
    const Type& get_type() const;

    Some_Node* get_initializer_node()
    {
        return m_children[1];
    }
    const Some_Node* get_initializer_node() const
    {
        return m_children[1];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::variable, get_position(), get_name() };
    }
};

struct Static_Assert final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Static_Assert";
    static inline constexpr std::string_view child_names[] = { "expression" };
    static inline constexpr bool is_expression = false;

    Static_Assert(Some_Node& parent, const astp::Static_Assert& parsed, std::string_view file);

    Some_Node* get_expression_node()
    {
        return m_children[0];
    }
    const Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::static_assertion, get_position() };
    }
};

struct If_Statement final : detail::Node_Base, detail::Annotated, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Statement";
    static inline constexpr std::string_view child_names[] = {
        "condition",
        "if_block",
        "else_block",
    };
    static inline constexpr bool is_expression = false;

    If_Statement(Some_Node& parent,
                 const astp::If_Statement& parsed,
                 std::string_view file,
                 std::span<const astp::Handle> annotations);

    Some_Node* get_condition_node()
    {
        return m_children[0];
    }
    const Some_Node* get_condition_node() const
    {
        return m_children[0];
    }

    Some_Node* get_if_block_node()
    {
        return m_children[1];
    }
    const Some_Node* get_if_block_node() const
    {
        return m_children[1];
    }

    Block_Statement& get_if_block();
    const Block_Statement& get_if_block() const;

    Some_Node* get_else_node()
    {
        return m_children[2];
    }
    const Some_Node* get_else_node() const
    {
        return m_children[2];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::if_statement, get_position() };
    }
};

struct While_Statement final : detail::Node_Base, detail::Annotated, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "While_Statement";
    static inline constexpr std::string_view child_names[] = { "condition", "block" };
    static inline constexpr bool is_expression = false;

    While_Statement(Some_Node& parent,
                    const astp::While_Statement& parsed,
                    std::string_view file,
                    std::span<const astp::Handle> annotations);

    Some_Node* get_condition_node()
    {
        return m_children[0];
    }
    const Some_Node* get_condition_node() const
    {
        return m_children[0];
    }

    Some_Node* get_block_node()
    {
        return m_children[1];
    }
    const Some_Node* get_block_node() const
    {
        return m_children[1];
    }

    Block_Statement& get_block();
    const Block_Statement& get_block() const;

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::while_statement, get_position() };
    }
};

enum struct Control_Statement_Type { return_, break_, continue_ };

[[nodiscard]] constexpr std::string_view
control_statement_type_code_name(Control_Statement_Type type)
{
    using enum Control_Statement_Type;
    switch (type) {
    case return_: return "return";
    case break_: return "break";
    case continue_: return "continue";
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type.");
}

[[nodiscard]] constexpr Token_Type control_statement_type_token(Control_Statement_Type type)
{
    using enum Control_Statement_Type;
    switch (type) {
    case return_: return Token_Type::keyword_return;
    case break_: return Token_Type::keyword_break;
    case continue_: return Token_Type::keyword_continue;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type.");
}

[[nodiscard]] constexpr Construct control_statement_type_construct(Control_Statement_Type type)
{
    using enum Control_Statement_Type;
    switch (type) {
    case return_: return Construct::return_statement;
    case break_: return Construct::break_statement;
    case continue_: return Construct::continue_statement;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid type.");
}

struct Control_Statement final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Control_Statement";
    static inline constexpr std::string_view child_names[] = { "expression" };
    static inline constexpr bool is_expression = false;

private:
    Control_Statement_Type m_type;

public:
    Control_Statement(Some_Node& parent,
                      const astp::Return_Statement& parsed,
                      std::string_view file);

    Control_Statement(Some_Node& parent, const astp::Break& parsed, std::string_view file);

    Control_Statement(Some_Node& parent, const astp::Continue& parsed, std::string_view file);

    [[nodiscard]] Control_Statement_Type get_type() const
    {
        return m_type;
    }

    [[nodiscard]] bool is_break() const
    {
        return m_type == Control_Statement_Type::break_;
    }
    [[nodiscard]] bool is_continue() const
    {
        return m_type == Control_Statement_Type::continue_;
    }
    [[nodiscard]] bool is_return() const
    {
        return m_type == Control_Statement_Type::return_;
    }

    Some_Node* get_expression_node()
    {
        return m_children[0];
    }
    const Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { control_statement_type_construct(get_type()), get_position() };
    }
};

struct Assignment final : detail::Node_Base, detail::Annotated, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Assignment";
    static inline constexpr std::string_view child_names[] = { "expression" };
    static inline constexpr bool is_expression = false;

private:
    std::string_view m_name;

public:
    Optional_Lookup_Result lookup_result {};

    Assignment(Some_Node& parent,
               const astp::Assignment& parsed,
               std::string_view file,
               std::span<const astp::Handle> annotations);

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_expression_node()
    {
        return m_children[0];
    }
    const Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::assignment, get_position(), get_name() };
    }
};

struct Block_Statement final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Block_Statement";
    static inline constexpr bool is_expression = false;

    Block_Statement(Some_Node& parent,
                    const astp::Block_Statement& parsed,
                    std::string_view file,
                    std::pmr::memory_resource* memory);

    bool is_empty() const
    {
        return m_children.empty();
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::block_statement, get_position() };
    }
};

struct Conversion_Expression final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Conversion_Expression";
    static inline constexpr std::string_view child_names[] = { "expression", "target_type" };
    static inline constexpr bool is_expression = true;

    Conversion_Expression(Some_Node& parent,
                          const astp::Conversion_Expression& parsed,
                          std::string_view file);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return Expression_Type::conversion;
    }

    Some_Node* get_expression_node()
    {
        return m_children[0];
    }
    const Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    Some_Node* get_target_type_node()
    {
        return m_children[1];
    }
    const Some_Node* get_target_type_node() const
    {
        return m_children[1];
    }

    Type& get_target_type();
    const Type& get_target_type() const;

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::conversion_expression, get_position() };
    }
};

struct If_Expression final : detail::Node_Base, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "condition", "right" };
    static inline constexpr bool is_expression = true;

    If_Expression(Some_Node& parent, const astp::If_Expression& parsed, std::string_view file);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return Expression_Type::if_expression;
    }

    Some_Node* get_left_node()
    {
        return m_children[0];
    }

    const Some_Node* get_left_node() const
    {
        return m_children[0];
    }

    Some_Node* get_condition_node()
    {
        return m_children[1];
    }
    const Some_Node* get_condition_node() const
    {
        return m_children[1];
    }

    Some_Node* get_right_node()
    {
        return m_children[2];
    }
    const Some_Node* get_right_node() const
    {
        return m_children[2];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::if_expression, get_position() };
    }
};

struct Binary_Expression final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Binary_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "right" };
    static inline constexpr bool is_expression = true;

private:
    Token_Type m_op;
    Expression_Type m_type;

public:
    Binary_Expression(Some_Node& parent,
                      const astp::Binary_Expression& parsed,
                      std::string_view file);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return m_type;
    }

    Token_Type get_op() const
    {
        return m_op;
    }

    Some_Node* get_left_node()
    {
        return m_children[0];
    }
    const Some_Node* get_left_node() const
    {
        return m_children[0];
    }

    Some_Node* get_right_node()
    {
        return m_children[1];
    }
    const Some_Node* get_right_node() const
    {
        return m_children[1];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::binary_expression, get_position() };
    }
};

struct Prefix_Expression final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Prefix_Expression";
    static inline constexpr std::string_view child_names[] = { "expression" };
    static inline constexpr bool is_expression = true;

private:
    Token_Type m_op;
    Expression_Type m_type;

public:
    Prefix_Expression(Some_Node& parent,
                      const astp::Prefix_Expression& parsed,
                      std::string_view file);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return m_type;
    }

    Token_Type get_op() const
    {
        return m_op;
    }

    Some_Node* get_expression_node()
    {
        return m_children[0];
    }
    const Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::prefix_expression, get_position() };
    }
};

struct Function_Call_Expression final : detail::Node_Base,
                                        detail::Annotated,
                                        detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Function_Call_Expression";
    static inline constexpr bool is_expression = true;

private:
    std::string_view m_name;
    bool m_is_statement;

public:
    Optional_Lookup_Result lookup_result {};

    Function_Call_Expression(Some_Node& parent,
                             const astp::Function_Call_Expression& parsed,
                             std::string_view file,
                             std::pmr::memory_resource* memory,
                             std::span<const astp::Handle> annotations);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return Expression_Type::function_call;
    }

    std::string_view get_name() const
    {
        return m_name;
    }

    Size get_argument_count() const
    {
        return m_children.size();
    }

    std::span<Some_Node*> get_argument_nodes()
    {
        return m_children;
    }
    std::span<const Some_Node* const> get_argument_nodes() const
    {
        return m_children;
    }

    Some_Node* get_argument_node(Size i)
    {
        BIT_MANIPULATION_ASSERT(i < m_children.size());
        return m_children[i];
    }
    const Some_Node* get_argument_node(Size i) const
    {
        BIT_MANIPULATION_ASSERT(i < m_children.size());
        return m_children[i];
    }

    bool is_statement() const
    {
        return m_is_statement;
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::function_call_expression, get_position(), get_name() };
    }
};

struct Id_Expression final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Id_Expression";
    static inline constexpr bool is_expression = true;

private:
    std::string_view m_identifier;

public:
    Optional_Lookup_Result lookup_result {};
    bool bit_generic = false;

    Id_Expression(Some_Node& parent, const astp::Id_Expression& parsed, std::string_view file);

    [[nodiscard]] Expression_Type get_expression_type() const
    {
        return Expression_Type::id;
    }

    std::string_view get_identifier() const
    {
        return m_identifier;
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::id_expression, get_position(), get_identifier() };
    }
};

struct Literal final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Literal";
    static inline constexpr bool is_expression = true;

private:
    std::string_view m_literal;
    Token_Type m_type;

public:
    Literal(Some_Node& parent, const astp::Literal& parsed, std::string_view file);

    Expression_Type get_expression_type() const
    {
        return Expression_Type::literal;
    }

    Token_Type get_type() const
    {
        return m_type;
    }

    std::string_view get_literal() const
    {
        return m_literal;
    }

    [[nodiscard]] Debug_Info get_debug_info() const
    {
        return { Construct::literal, get_position() };
    }
};

using Some_Node_Variant = Variant<Program,
                                  Function,
                                  Type,
                                  Const,
                                  Let,
                                  Static_Assert,
                                  If_Statement,
                                  While_Statement,
                                  Control_Statement,
                                  Assignment,
                                  Block_Statement,
                                  Conversion_Expression,
                                  If_Expression,
                                  Binary_Expression,
                                  Prefix_Expression,
                                  Function_Call_Expression,
                                  Id_Expression,
                                  Literal>;

struct Some_Node : Some_Node_Variant {
    using Variant::Variant;
};

static_assert(std::is_move_constructible_v<Some_Node>);
static_assert(std::is_copy_constructible_v<Some_Node>);

inline Function& Function::Instance::get_function()
{
    return get<Function>(*handle);
}

inline const Function& Function::Instance::get_function() const
{
    return get<Function>(*handle);
}

inline Type& Function::get_return_type()
{
    BIT_MANIPULATION_ASSERT(get_return_type_node());
    return get<Type>(*get_return_type_node());
}
inline const Type& Function::get_return_type() const
{
    BIT_MANIPULATION_ASSERT(get_return_type_node());
    return get<Type>(*get_return_type_node());
}

inline Block_Statement& Function::get_body()
{
    BIT_MANIPULATION_ASSERT(get_body_node());
    return get<Block_Statement>(*get_body_node());
}
inline const Block_Statement& Function::get_body() const
{
    BIT_MANIPULATION_ASSERT(get_body_node());
    return get<Block_Statement>(*get_body_node());
}

inline Debug_Info Function::get_return_type_debug_info() const
{
    if (m_return_type) {
        return get_return_type().get_debug_info();
    }
    else {
        return { Construct::implicit_type, get_position() };
    }
}

inline Concrete_Type Function::get_concrete_return_type() const
{
    BIT_MANIPULATION_ASSERT(!is_generic);
    if (!m_return_type) {
        return Concrete_Type::Void;
    }
    const ast::Type& type = get_return_type();
    BIT_MANIPULATION_ASSERT(type.was_analyzed());
    return type.concrete_type().value();
}

inline Type& Let::get_type()
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}
inline const Type& Let::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}

inline Type& Const::get_type()
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}
inline const Type& Const::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}

inline Block_Statement& If_Statement::get_if_block()
{
    BIT_MANIPULATION_ASSERT(get_if_block_node());
    return get<Block_Statement>(*get_if_block_node());
}
inline const Block_Statement& If_Statement::get_if_block() const
{
    BIT_MANIPULATION_ASSERT(get_if_block_node());
    return get<Block_Statement>(*get_if_block_node());
}

inline Block_Statement& While_Statement::get_block()
{
    BIT_MANIPULATION_ASSERT(get_block_node());
    return get<Block_Statement>(*get_block_node());
}
inline const Block_Statement& While_Statement::get_block() const
{
    BIT_MANIPULATION_ASSERT(get_block_node());
    return get<Block_Statement>(*get_block_node());
}

inline Type& Conversion_Expression::get_target_type()
{
    BIT_MANIPULATION_ASSERT(get_target_type_node());
    return get<Type>(*get_target_type_node());
}
inline const Type& Conversion_Expression::get_target_type() const
{
    BIT_MANIPULATION_ASSERT(get_target_type_node());
    return get<Type>(*get_target_type_node());
}

template <int N>
template <std::forward_iterator Forward_It, std::forward_iterator Sentinel>
void detail::Parent<N>::set_children(Forward_It begin, Sentinel end)
{
    BIT_MANIPULATION_ASSERT(end - begin == N);
    // This is std::ranges::copy but we don't want the dependency in this header.
    Some_Node** out = m_children;
    for (auto it = begin; it != end; ++it) {
        *out++ = *it;
    }
}

namespace detail {

inline std::span<Some_Node*> Dynamic_Parent::get_children()
{
    return m_children;
}

inline std::span<const Some_Node* const> Dynamic_Parent::get_children() const
{
    return m_children;
}

template <int N>
inline std::span<Some_Node*> Parent<N>::get_children()
{
    // not really sure why we can't just "return m_children", but clang doesn't accept that
    // https://stackoverflow.com/q/79264902/5740428
    return { m_children, std::size(m_children) };
}

template <int N>
inline std::span<const Some_Node* const> Parent<N>::get_children() const
{
    return { m_children, std::size(m_children) };
}

inline const Node_Base& to_node_base(const Some_Node& node) noexcept
{
    return visit([](const Node_Base& n) -> const Node_Base& { return n; }, node);
}

inline Node_Base& to_node_base(Some_Node& node) noexcept
{
    return visit([](Node_Base& n) -> Node_Base& { return n; }, node);
}

} // namespace detail

inline bool is_expression(const Some_Node& node)
{
    return visit([]<typename T>(const T&) { return T::is_expression; }, node);
}

inline Expression_Type get_expression_type(const Some_Node& node)
{
    return visit(
        []<typename T>(const T& n) -> Expression_Type { //
            if constexpr (T::is_expression) {
                return n.get_expression_type();
            }
            else {
                BIT_MANIPULATION_ASSERT_UNREACHABLE();
            }
        },
        node);
}

inline Debug_Info get_debug_info(const Some_Node& node)
{
    return visit([]<typename T>(const T& n) { return n.get_debug_info(); }, node);
}

inline std::string_view get_node_name(const Some_Node& node)
{
    return visit([]<typename T>(const T&) { return T::self_name; }, node);
}

inline Some_Node* get_parent(Some_Node& node)
{
    return detail::to_node_base(node).get_parent();
}

inline const Some_Node* get_parent(const Some_Node& node)
{
    return detail::to_node_base(node).get_parent();
}

inline void set_parent(Some_Node& node, Some_Node& parent)
{
    return detail::to_node_base(node).set_parent(parent);
}

inline std::optional<Source_Span> get_source_position(const Some_Node& node)
{
    return detail::to_node_base(node).get_position();
}

inline std::optional<Value>& get_const_value(Some_Node& node)
{
    return detail::to_node_base(node).const_value();
}

inline const std::optional<Value>& get_const_value(const Some_Node& node)
{
    return detail::to_node_base(node).const_value();
}

namespace detail {

inline constexpr struct {
    template <typename T>
    std::span<const_like_t<const_like_t<Some_Node, T>*, T>> operator()(T& n) const
    {
        if constexpr (requires { n.get_children(); }) {
            return n.get_children();
        }
        else {
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Node does not support child access.");
        }
    }
} do_get_children;

} // namespace detail

inline std::span<Some_Node*> get_children(Some_Node& node)
{
    return visit(detail::do_get_children, node);
}

inline std::span<const Some_Node* const> get_children(const Some_Node& node)
{
    return visit(detail::do_get_children, node);
}

template <alternative_of<ast::Some_Node_Variant> T>
const T* get_surrounding(const Some_Node& node) noexcept
{
    for (const Some_Node* p = get_parent(node); p != nullptr; p = get_parent(*p)) {
        if (const auto* f = get_if<T>(p)) {
            return f;
        }
    }
    return nullptr;
}

template <alternative_of<ast::Some_Node_Variant> T>
T* get_surrounding(Some_Node& node) noexcept
{
    return const_cast<T*>(get_surrounding<T>(std::as_const(node)));
}

} // namespace ast

inline ast::Type& Parameter::get_type()
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<ast::Type>(*get_type_node());
}
inline const ast::Type& Parameter::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<ast::Type>(*get_type_node());
}

} // namespace bit_manipulation::bms

#endif
