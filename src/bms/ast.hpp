#ifndef BIT_MANIPULATION_BMS_AST_HPP
#define BIT_MANIPULATION_BMS_AST_HPP

#include <optional>
#include <ranges>
#include <span>
#include <vector>

#include "common/variant.hpp"

#include "bms/analysis_error.hpp"
#include "bms/astp.hpp"
#include "bms/concrete_type.hpp"
#include "bms/deduction.hpp"
#include "bms/fwd.hpp"
#include "bms/tokens.hpp"
#include "bms/value.hpp"

namespace bit_manipulation::bms::ast {

namespace detail {

struct Node_Base {
public:
    [[nodiscard]] static Node_Base make_builtin() noexcept
    {
        return Node_Base {};
    }

protected:
    std::optional<Source_Span> m_position;
    /// @brief If present, indicates that type analysis for an AST node is complete.
    /// For some nodes such as expression nodes, this also indicates the value of the node, if it
    /// is a constant.
    std::optional<Value> m_const_value;

    explicit Node_Base()
        : m_const_value(Value::unknown_of_type(Concrete_Type::Void))
    {
    }

public:
    explicit Node_Base(const astp::detail::Node_Base& parsed, std::string_view file)
        : m_position(Source_Span { parsed.pos, file })
    {
    }

    explicit Node_Base(const Source_Span& pos, std::optional<Value> value = {})
        : m_position(pos)
        , m_const_value(value)
    {
    }

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
};

template <int N>
struct Parent {
protected:
    Some_Node* m_children[N] {};

public:
    std::span<Some_Node*> get_children()
    {
        return m_children;
    }

    std::span<Some_Node* const> get_children() const
    {
        return m_children;
    }

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

    std::span<Some_Node* const> get_children() const
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

    std::span<Some_Node* const> get_children() const;

    template <std::ranges::forward_range R>
    void set_children(R&& r)
    {
        m_children.clear();
        m_children.insert(m_children.begin(), r.begin(), r.end());
    }
};

} // namespace detail

struct Program final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Program";

    Program(const astp::Program& parsed, std::string_view file, std::pmr::memory_resource* memory)
        : detail::Node_Base(parsed, file)
        , detail::Dynamic_Parent(memory)
    {
    }
};

struct Function final : detail::Node_Base, detail::Parent<4> {
private:
    struct Copy_for_Instantiation_Tag { };

public:
    static inline constexpr std::string_view self_name = "Function";
    static inline constexpr std::string_view child_names[]
        = { "parameters", "return_type", "requires_clause", "body" };

    struct Instance {
        std::pmr::vector<int> widths;
        Some_Node* handle;

        bool has_widths(const Widths& w) const noexcept
        {
            for (Size i = 0; i < widths.size(); ++i) {
                if (widths[i] != get_width(w, i)) {
                    return false;
                }
            }
            return true;
        }
    };

    static inline constexpr Size invalid_vm_address = Size(-1);

private:
    std::string_view m_name;

public:
    std::pmr::vector<Instance> instances;
    bool is_generic = false;
    Size vm_address = invalid_vm_address;
    Analysis_Level analysis_so_far = Analysis_Level::unanalyzed;

private:
    Function(const Function& other, Copy_for_Instantiation_Tag);

public:
    Function(const astp::Function& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_name(parsed.name)
    {
    }

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_parameters_node() const
    {
        return m_children[0];
    }
    Parameter_List& get_parameters() const;

    Some_Node* get_return_type_node() const
    {
        return m_children[1];
    }
    Type& get_return_type() const;

    Some_Node* get_requires_clause_node() const
    {
        return m_children[2];
    }

    Some_Node* get_body_node() const
    {
        return m_children[3];
    }
    Block_Statement& get_body() const;

    Function copy_for_instantiation() const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        return { *this, Copy_for_Instantiation_Tag {} };
    }
    const Instance* find_instance(const Widths& w) const
    {
        BIT_MANIPULATION_ASSERT(is_generic);
        for (const Instance& instance : instances) {
            if (instance.has_widths(w)) {
                return &instance;
            }
        }
        return nullptr;
    }
};

// It is weird to have a dedicated type just for a `Parameter_List`, but there are a few design
// choices that have led to this.
// Most importantly, any node is able to expose its list of children as a `std::span`.
// Since a `Function` already has other nodes such as a return type, it would be tricky to make
// a function hold a parameter list followed by the other types, but not impossible.
struct Parameter_List final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Parameter_List";

    Parameter_List(const astp::Parameter_List& parsed,
                   std::string_view file,
                   std::pmr::memory_resource* memory)
        : detail::Node_Base(parsed, file)
        , detail::Dynamic_Parent(memory)
    {
    }
};

struct Parameter final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Parameter";
    static inline constexpr std::string_view child_names[] = { "type" };

private:
    std::string_view m_name;

public:
    Parameter(const astp::Parameter& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_name(parsed.name)
    {
    }

    std::string_view get_name()
    {
        return m_name;
    }

    Some_Node* get_type_node() const
    {
        return m_children[0];
    }
    Type& get_type() const;
};

struct Type final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Type";
    static inline constexpr std::string_view child_names[] = { "width" };

private:
    Type_Type m_type;

public:
    std::optional<int> concrete_width;

    Type(const astp::Type& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_type(parsed.type)
    {
    }

    Type_Type get_type() const
    {
        return m_type;
    }

    Some_Node* get_width_node() const
    {
        return m_children[0];
    }

    /// @brief Returns a concrete type based on the information in this node, or `std::nullopt`
    /// if the information is incomplete.
    /// Namely, if the type is `Uint`, a concrete width must be known, otherwise this function
    /// always succeeds.
    /// @return A concrete type, or `std::nullopt`.
    std::optional<Concrete_Type> concrete_type() const
    {
        if (m_type == Type_Type::Uint) {
            if (concrete_width) {
                return Concrete_Type::Uint(*concrete_width);
            }
            return std::nullopt;
        }
        // For anything but UInt, why would concrete_width be set?
        BIT_MANIPULATION_ASSERT(!concrete_width);
        return Concrete_Type { m_type };
    }
};

struct Const final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Const";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };

private:
    std::string_view m_name;

public:
    Const(const astp::Const& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_name(parsed.name)
    {
    }

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_type_node() const
    {
        return m_children[0];
    }
    Type& get_type() const;

    Some_Node* get_initializer_node() const
    {
        return m_children[1];
    }
};

struct Let final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Let";
    static inline constexpr std::string_view child_names[] = { "type", "initializer" };

private:
    std::string_view m_name;

public:
    Let(const astp::Let& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_name(parsed.name) {};

    std::string_view get_name() const
    {
        return m_name;
    }

    Some_Node* get_type_node() const
    {
        return m_children[0];
    }
    Type& get_type() const;

    Some_Node* get_initializer_node() const
    {
        return m_children[1];
    }
};

struct Static_Assert final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Static_Assert";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Static_Assert(const astp::Static_Assert& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_expression_node() const
    {
        return m_children[0];
    }
};

struct If_Statement final : detail::Node_Base, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Statement";
    static inline constexpr std::string_view child_names[] = {
        "condition",
        "if_block",
        "else_block",
    };

    If_Statement(const astp::If_Statement& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_condition_node() const
    {
        return m_children[0];
    }

    Some_Node* get_if_block_node() const
    {
        return m_children[1];
    }
    Block_Statement& get_if_block() const;

    Some_Node* get_else_block_node() const
    {
        return m_children[2];
    }
    Block_Statement& get_else_block() const;
};

struct While_Statement final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "While_Statement";
    static inline constexpr std::string_view child_names[] = { "condition", "block" };

    While_Statement(const astp::While_Statement& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_condition_node() const
    {
        return m_children[0];
    }

    Some_Node* get_block_node() const
    {
        return m_children[1];
    }
    Block_Statement& get_block() const;
};

struct Break final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Break";

    Break(const astp::Break& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }
};

struct Continue final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Continue";

    Continue(const astp::Continue& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }
};

struct Return_Statement final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Return_Statement";
    static inline constexpr std::string_view child_names[] = { "expression" };

    Return_Statement(const astp::Return_Statement& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_expression_node() const
    {
        return m_children[0];
    }
};

struct Assignment final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Assignment";
    static inline constexpr std::string_view child_names[] = { "expression" };

    std::string_view name;
    Some_Node* lookup_result = nullptr;

    Assignment(const astp::Assignment& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , name(parsed.name)
    {
    }

    Some_Node* get_expression_node() const
    {
        return m_children[0];
    }
};

struct Block_Statement final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Block_Statement";

    Block_Statement(const astp::Block_Statement& parsed,
                    std::string_view file,
                    std::pmr::memory_resource* memory)
        : detail::Node_Base(parsed, file)
        , detail::Dynamic_Parent(memory)
    {
    }
};

struct Conversion_Expression final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Conversion_Expression";
    static inline constexpr std::string_view child_names[] = { "expression", "target_type" };

    Conversion_Expression(const astp::Conversion_Expression& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_expression_node() const
    {
        return m_children[0];
    }

    Some_Node* get_target_type_node() const
    {
        return m_children[1];
    }
    Type& get_target_type() const;
};

struct If_Expression final : detail::Node_Base, detail::Parent<3> {
    static inline constexpr std::string_view self_name = "If_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "condition", "right" };

    If_Expression(const astp::If_Expression& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
    {
    }

    Some_Node* get_left_node() const
    {
        return m_children[0];
    }
    Some_Node* get_condition_node() const
    {
        return m_children[1];
    }
    Some_Node* get_right_node() const
    {
        return m_children[2];
    }
};

struct Binary_Expression final : detail::Node_Base, detail::Parent<2> {
    static inline constexpr std::string_view self_name = "Binary_Expression";
    static inline constexpr std::string_view child_names[] = { "left", "right" };

private:
    Token_Type m_op;

public:
    Binary_Expression(const astp::Binary_Expression& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_op(parsed.op)
    {
    }

    Token_Type get_op() const
    {
        return m_op;
    }

    Some_Node* get_left_node() const
    {
        return m_children[0];
    }
    Some_Node* get_right_node() const
    {
        return m_children[1];
    }
};

struct Prefix_Expression final : detail::Node_Base, detail::Parent<1> {
    static inline constexpr std::string_view self_name = "Prefix_Expression";
    static inline constexpr std::string_view child_names[] = { "expression" };

private:
    Token_Type m_op;

public:
    Prefix_Expression(const astp::Prefix_Expression& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_op(parsed.op)
    {
    }

    Token_Type get_op() const
    {
        return m_op;
    }

    Some_Node* get_expression_node() const
    {
        return m_children[0];
    }
};

struct Function_Call_Expression final : detail::Node_Base, detail::Dynamic_Parent {
    static inline constexpr std::string_view self_name = "Function_Call_Expression";

private:
    std::string_view m_name;
    bool m_is_statement;

public:
    Some_Node* lookup_result = nullptr;

    Function_Call_Expression(const astp::Function_Call_Expression& parsed,
                             std::string_view file,
                             std::pmr::memory_resource* memory)
        : detail::Node_Base(parsed, file)
        , detail::Dynamic_Parent(memory)
        , m_name(parsed.function)
        , m_is_statement(parsed.is_statement)
    {
    }

    std::string_view get_name() const
    {
        return m_name;
    }

    bool is_statement() const
    {
        return m_is_statement;
    }
};

struct Id_Expression final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Id_Expression";

private:
    std::string_view m_identifier;

public:
    Some_Node* lookup_result = nullptr;
    bool bit_generic = false;

    Id_Expression(const astp::Id_Expression& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_identifier(parsed.identifier)
    {
    }

    std::string_view get_identifier() const
    {
        return m_identifier;
    }
};

struct Literal final : detail::Node_Base, detail::Parent<0> {
    static inline constexpr std::string_view self_name = "Literal";

private:
    std::string_view m_literal;
    Token_Type m_type;

public:
    Literal(const astp::Literal& parsed, std::string_view file)
        : detail::Node_Base(parsed, file)
        , m_literal(parsed.literal)
        , m_type(parsed.type)
    {
    }

    Token_Type get_type() const
    {
        return m_type;
    }

    std::string_view get_literal() const
    {
        return m_literal;
    }
};

struct Builtin_Function final : detail::Node_Base, detail::Parent<0> {
public:
    static inline constexpr std::string_view self_name = "Builtin_Function";

    bms::Builtin_Function m_function;

public:
    Builtin_Function(bms::Builtin_Function function)
        : detail::Node_Base(detail::Node_Base::make_builtin())
        , m_function(function)
    {
    }

    bms::Builtin_Function get_function() const
    {
        return m_function;
    }
};

using Some_Node_Variant = Variant<Program,
                                  Function,
                                  Parameter_List,
                                  Parameter,
                                  Type,
                                  Const,
                                  Let,
                                  Static_Assert,
                                  If_Statement,
                                  While_Statement,
                                  Break,
                                  Continue,
                                  Return_Statement,
                                  Assignment,
                                  Block_Statement,
                                  Conversion_Expression,
                                  If_Expression,
                                  Binary_Expression,
                                  Prefix_Expression,
                                  Function_Call_Expression,
                                  Id_Expression,
                                  Literal,
                                  Builtin_Function>;

struct Some_Node : Some_Node_Variant {
    using Variant::Variant;
};

inline Parameter_List& Function::get_parameters() const
{
    BIT_MANIPULATION_ASSERT(get_parameters_node());
    return get<Parameter_List>(*get_parameters_node());
}

inline Type& Function::get_return_type() const
{
    BIT_MANIPULATION_ASSERT(get_return_type_node());
    return get<Type>(*get_return_type_node());
}

inline Block_Statement& Function::get_body() const
{
    BIT_MANIPULATION_ASSERT(get_body_node());
    return get<Block_Statement>(*get_body_node());
}

inline Type& Parameter::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}

inline Type& Let::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}

inline Type& Const::get_type() const
{
    BIT_MANIPULATION_ASSERT(get_type_node());
    return get<Type>(*get_type_node());
}

inline Block_Statement& If_Statement::get_if_block() const
{
    BIT_MANIPULATION_ASSERT(get_if_block_node());
    return get<Block_Statement>(*get_if_block_node());
}

inline Block_Statement& If_Statement::get_else_block() const
{
    BIT_MANIPULATION_ASSERT(get_else_block_node());
    return get<Block_Statement>(*get_else_block_node());
}

inline Block_Statement& While_Statement::get_block() const
{
    BIT_MANIPULATION_ASSERT(get_block_node());
    return get<Block_Statement>(*get_block_node());
}

inline Type& Conversion_Expression::get_target_type() const
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

inline std::span<Some_Node* const> Dynamic_Parent::get_children() const
{
    return m_children;
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

inline std::string_view get_node_name(const Some_Node& node)
{
    return visit([]<typename T>(const T&) { return T::self_name; }, node);
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

inline std::span<Some_Node*> get_children(Some_Node& node)
{
    return visit([](auto& n) { return n.get_children(); }, node);
}

inline std::span<Some_Node* const> get_children(const Some_Node& node)
{
    return visit([](auto& n) { return n.get_children(); }, node);
}

} // namespace bit_manipulation::bms::ast

#endif
