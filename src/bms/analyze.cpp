#include <unordered_map>

#include "common/parse.hpp"

#include "bms/analysis_error.hpp"
#include "bms/analyze.hpp"
#include "bms/ast.hpp"
#include "bms/operations.hpp"
#include "bms/parse.hpp"
#include "bms/vm.hpp"
#include "bms/vm_codegen.hpp"
#include "vm_instructions.hpp"

namespace bit_manipulation::bms {

namespace {

[[nodiscard]] Concrete_Value concrete_value_of(const ast::Some_Node& node)
{
    return get_const_value(node).value().concrete_value();
}

[[nodiscard]] Comparison_Failure comparison_failure_of(const ast::Binary_Expression& expression)
{
    const Concrete_Value l = concrete_value_of(*expression.get_left());
    const Concrete_Value r = concrete_value_of(*expression.get_right());
    return { l, r, expression.get_op() };
}

template <typename T>
concept Lookup_Performing_Node = requires(T& t) {
    { t.lookup_result } -> std::same_as<ast::Some_Node*&>;
};

static_assert(Lookup_Performing_Node<ast::Id_Expression>);

/// @brief Class responsible for performing instantiations.
/// After runnings this analyzer, functions will have attached instance nodes.
/// These instance nodes are clones of the original function node but where the bit-generic
/// parameter types are replaced with concrete types.
/// Also, the id-expression within the bit-generic type will have a constant value, which
/// facilitates constant propagation in further stages.
struct Instantiator {
private:
    Analyzed_Program& m_program;
    std::pmr::monotonic_buffer_resource m_memory_resource;
    std::pmr::unordered_map<const ast::Some_Node*, ast::Some_Node*> m_remap;

public:
    Instantiator(Analyzed_Program& program, std::pmr::memory_resource* memory)
        : m_program(program)
        , m_memory_resource(memory)
        , m_remap(&m_memory_resource)
    {
    }

    /// @brief Instantiates every bit-generic function in the program using the same `Widths`.
    /// This is mostly useful for testing purposes and cannot be triggered "naturally" through
    /// implicit instantiations.
    Result<void, Analysis_Error> instantiate_all(const Widths& w)
    {
        for (ast::Some_Node* decl : get_children(*m_program.get_root())) {
            if (auto* f = std::get_if<ast::Function>(decl)) {
                auto r = instantiate_function(decl, *f, w);
                if (!r) {
                    return r.error();
                }
            }
        }
        return {};
    }

    /// @brief Instantiates an individual function.
    /// Contrary to `instantiate_all`, this may be necessitated by implicit instantiations,
    /// for example when a function taking `Uint(N)` is called.
    ///
    /// At a high level, the process of instantiation works as follows:
    ///   1. Make a `deep_copy` of the node to be instantiated
    ///   2. `substitute_widths` to replace occurrences of bit-generic parameters with constants
    ///   3. Append the resulting `ast::Function::Instance` to the existing node
    /// @param h the pointer to the function node
    /// @param node the function node
    /// @param w the widths for instantiation
    /// @return the instantiated instance, or `Analysis_Error`
    Result<const ast::Function::Instance*, Analysis_Error>
    instantiate_function(ast::Some_Node* h, ast::Function& node, const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(node.is_generic);

        if (const ast::Function::Instance* const existing = node.find_instance(w)) {
            return existing;
        }

        ast::Some_Node* instance = deep_copy(h);
        auto& instance_node = std::get<ast::Function>(*instance);
        Result<std::pmr::vector<int>, Analysis_Error> effective_widths
            = substitute_widths(instance_node, w);
        if (!effective_widths) {
            return effective_widths.error();
        }
        return &node.instances.emplace_back(std::move(*effective_widths), instance);
    }

private:
    Result<std::pmr::vector<int>, Analysis_Error> substitute_widths(ast::Function& instance,
                                                                    const Widths& w)
    {
        BIT_MANIPULATION_ASSERT(!instance.is_generic);

        std::pmr::vector<int> widths(m_program.get_memory_resource());
        const auto next_width = [&widths, &w] {
            const int result = get_width(w, widths.size());
            widths.push_back(result);
            return result;
        };

        auto& params = std::get<ast::Parameter_List>(*instance.get_parameters());
        for (ast::Some_Node* p : params.get_children()) {
            auto& param_node = std::get<ast::Parameter>(*p);
            auto& type_node = std::get<ast::Type>(*param_node.get_type());
            if (type_node.get_width() == nullptr) {
                continue;
            }
            BIT_MANIPULATION_ASSERT(type_node.get_type() == Type_Type::Uint);
            auto* id_node = std::get_if<ast::Id_Expression>(type_node.get_width());
            // If it isn't an id-expression, it cannot be a generic parameter.
            if (id_node && id_node->bit_generic) {
                const int width = next_width();
                // Merely replacing the type isn't enough; we must also give this node a
                // constant value so that existing name lookup can obtain this value.
                id_node->const_value() = Value::Int(width);
                type_node.concrete_width = width;
            }
        }

        return widths;
    }

    ast::Some_Node* deep_copy(const ast::Some_Node* h)
    {
        // Two passes are necessary.
        // During the first pass, the nodes are all copied and the links to children are updated
        // in the copies.
        // However, this does not yet affect the name lookup results, which must also refer to
        // the cloned nodes.
        // This is done in a second pass, since cloning a cyclic graph in one go is difficult.

        m_remap.clear();
        ast::Some_Node* const result = deep_copy_for_instantiation(h);
        deep_update_name_lookup(result);

        return result;
    }

    ast::Some_Node* deep_copy_for_instantiation(const ast::Some_Node* h)
    {
        if (h == nullptr) {
            return nullptr;
        }
        ast::Some_Node* const result = copy_single_node_for_instantiation(h);
        m_remap.emplace(h, result);

        for (ast::Some_Node*& child : get_children(*result)) {
            child = deep_copy_for_instantiation(child);
        }
        return result;
    }

    void deep_update_name_lookup(ast::Some_Node* h)
    {
        if (h == nullptr) {
            return;
        }
        fast_visit(
            [this, &h]<typename T>(T& n) {
                if constexpr (std::is_same_v<T, ast::Id_Expression>) {
                    BIT_MANIPULATION_ASSERT(n.bit_generic || n.lookup_result != nullptr);
                }
                if constexpr (Lookup_Performing_Node<T>) {
                    if (n.lookup_result != nullptr) {
                        n.lookup_result = m_remap.at(n.lookup_result);
                    }
                }
            },
            *h);
        for (ast::Some_Node* const child : get_children(*h)) {
            deep_update_name_lookup(child);
        }
    }

    ast::Some_Node* copy_single_node_for_instantiation(const ast::Some_Node* h)
    {
        BIT_MANIPULATION_ASSERT(h != nullptr);
        return fast_visit(
            [this]<typename T>(const T& n) {
                if constexpr (requires { n.copy_for_instantiation(); }) {
                    return m_program.insert(n.copy_for_instantiation());
                }
                else {
                    return m_program.insert(n);
                }
            },
            *h);
    }
};

// =================================================================================================

enum struct Expression_Context : Default_Underlying {
    // The default context. Full analysis of functions and everything inside.
    normal,
    // A constant expression. Constant evaluation cannot run into an unknown value.
    constant,
};

struct Type_Analyzer {
private:
    struct Function_Info {
        ast::Function* function;
        Concrete_Type return_type;
        ast::Some_Node* return_type_node;
    };
    Analyzed_Program& m_program;
    std::pmr::unsynchronized_pool_resource m_memory_resource;
    Virtual_Machine constant_evaluation_machine;
    std::vector<Function_Info> m_function_stack;

    struct Function_Stack_Scope {
        std::vector<Function_Info>& m_function_stack;
        ~Function_Stack_Scope()
        {
            m_function_stack.pop_back();
        }
    };

    [[nodiscard]] Function_Stack_Scope push_function(const Function_Info& info)
    {
        m_function_stack.push_back(info);
        return { m_function_stack };
    }

    const Function_Info& top_function() const
    {
        BIT_MANIPULATION_ASSERT(!m_function_stack.empty());
        return m_function_stack.back();
    }

public:
    Type_Analyzer(Analyzed_Program& program, std::pmr::memory_resource* memory)
        : m_program(program)
        , m_memory_resource(memory)
        , constant_evaluation_machine(&m_memory_resource)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        return analyze_types(m_program.get_root(), Analysis_Level::full,
                             Expression_Context::normal);
    }

private:
    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, Analysis_Level level, Expression_Context context)
    {
        if (handle == nullptr) {
            return {};
        }
        return fast_visit(
            [this, handle, level, context](auto& node) {
                return analyze_types(handle, node, level, context); //
            },
            *handle);
    }

    template <typename T>
        requires(!std::is_const_v<T>)
    Result<void, Analysis_Error>
    analyze_child_types(T& node, Analysis_Level level, Expression_Context context)
    {
        for (ast::Some_Node* h : node.get_children()) {
            auto r = analyze_types(h, level, context);
            if (!r) {
                return r;
            }
        }
        node.const_value() = Value::Void;
        return {};
    }

    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Program& node, Analysis_Level level, Expression_Context)
    {
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Function& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        // Normally we don't check, but this is tricky to ensure with implicit instantiations etc.
        BIT_MANIPULATION_ASSERT(&std::get<ast::Function>(*handle) == &node);
        BIT_MANIPULATION_ASSERT(level != Analysis_Level::unanalyzed);
        if (node.analysis_so_far >= level) {
            return {};
        }

        // If the function is generic, we can never say that analysis succeeded entirely because
        // new implicit instantiations may have been generated in the meantime.
        // For concrete functions, we can memoize the result of type analysis.
        if (node.is_generic) {
            for (const ast::Function::Instance& instance : node.instances) {
                auto r = analyze_types(instance.handle, level, Expression_Context::normal);
                if (!r) {
                    return r;
                }
            }
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.instances.empty());

        if (auto r = analyze_types(node.get_parameters(), level, Expression_Context::normal); !r) {
            return r;
        }
        if (auto r = analyze_types(node.get_return_type(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto& return_type = std::get<ast::Type>(*node.get_return_type());
        auto scope = push_function({ .function = &node,
                                     .return_type = return_type.const_value()->get_type(),
                                     .return_type_node = node.get_return_type() });

        if (node.get_requires_clause() != nullptr) {
            auto& expr_const_value = get_const_value(*node.get_requires_clause());
            // Expressions in requires-clauses don't need to be checked twice, but we have to
            // determine this here instead of in the expression because expressions lack a
            // checking mechanism for themselves.
            if (!expr_const_value) {
                if (auto r = analyze_types(node.get_requires_clause(),
                                           Analysis_Level::for_constant_evaluation,
                                           Expression_Context::constant);
                    !r) {
                    return r;
                }
                node.const_value() = expr_const_value;
                BIT_MANIPULATION_ASSERT(node.const_value() && node.const_value()->is_known());

                if (node.const_value()->get_type() != Concrete_Type::Bool) {
                    return Analysis_Error { Analysis_Error_Code::requires_clause_not_bool,
                                            node.get_requires_clause(), handle };
                }
                if (!node.const_value()->as_bool()) {
                    return Analysis_Error { Analysis_Error_Code::requires_clause_not_satisfied,
                                            node.get_requires_clause(), handle };
                }
            }
        }

        if (node.analysis_so_far < Analysis_Level::shallow) {
            node.analysis_so_far = Analysis_Level::shallow;
        }
        if (level >= Analysis_Level::full) {
            if (auto r = analyze_types(node.get_body(), level, Expression_Context::normal); !r) {
                return r;
            }
        }
        if (level >= Analysis_Level::for_constant_evaluation) {
            const Size vm_address = constant_evaluation_machine.instruction_count();
            Result<void, Analysis_Error> instructions
                = generate_code(constant_evaluation_machine.instructions(), node);
            if (!instructions) {
                return instructions.error();
            }
            node.vm_address = vm_address;
        }
        BIT_MANIPULATION_ASSERT(!node.is_generic);
        node.const_value() = get_const_value(*node.get_return_type());
        node.analysis_so_far = level;
        return {};
    }

    [[gnu::always_inline]] Result<void, Analysis_Error> analyze_types(ast::Some_Node*,
                                                                      ast::Parameter_List& node,
                                                                      Analysis_Level level,
                                                                      Expression_Context)
    {
        if (node.const_value()) {
            return {};
        }
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Parameter& node, Analysis_Level level, Expression_Context)
    {
        if (node.const_value()) {
            return {};
        }
        auto& type = std::get<ast::Type>(*node.get_type());
        auto r = analyze_types(node.get_type(), type, level, Expression_Context::normal);
        if (!r) {
            return r;
        }
        BIT_MANIPULATION_ASSERT(type.const_value());
        node.const_value() = type.const_value();
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Type& node, Analysis_Level, Expression_Context)
    {
        if (node.const_value()) {
            return {};
        }
        if (node.get_width() == nullptr) {
            node.const_value() = Value::unknown_of_type(node.concrete_type().value());
            return {};
        }

        if (auto r = analyze_types(node.get_width(), Analysis_Level::for_constant_evaluation,
                                   Expression_Context::constant);
            !r) {
            return r;
        }
        auto width = get_const_value(*node.get_width());
        BIT_MANIPULATION_ASSERT(width.has_value());
        if (!width->get_type().is_integer()) {
            return Analysis_Error { Analysis_Error_Code::width_not_integer, node.get_width() };
        }
        const Big_Int folded_width = width->as_int();
        if (folded_width == 0) {
            return Analysis_Error { Analysis_Error_Code::width_zero, node.get_width() };
        }
        if (folded_width > uint_max_width) {
            return Analysis_Error { Analysis_Error_Code::width_too_large, node.get_width() };
        }
        node.concrete_width = static_cast<int>(folded_width);
        node.const_value() = Value::unknown_of_type(node.concrete_type().value());
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Const& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        // Const nodes contain constant expressions, so if they have been analyzed in the past,
        // we can be sure that no further analysis is required.
        if (node.const_value()) {
            return {};
        }
        // Const nodes must always have an initializer.
        BIT_MANIPULATION_ASSERT(node.get_initializer() != nullptr);

        if (node.get_type() == nullptr) {
            if (auto r
                = analyze_types(node.get_initializer(), Analysis_Level::for_constant_evaluation,
                                Expression_Context::constant);
                !r) {
                return r;
            }
            node.const_value() = get_const_value(*node.get_initializer());
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }

        auto& type_node = std::get<ast::Type>(*node.get_type());
        const auto type_result
            = analyze_types(node.get_type(), type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }

        if (const auto r
            = analyze_types(node.get_initializer(), Analysis_Level::for_constant_evaluation,
                            Expression_Context::constant);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(*node.get_initializer());

        BIT_MANIPULATION_ASSERT(type_node.const_value().has_value());
        if (!initializer_value->get_type().is_convertible_to(type_node.const_value()->get_type())) {
            return Analysis_Error { Analysis_Error_Code::incompatible_types, handle,
                                    node.get_initializer() };
        }
        const Result<Value, Conversion_Error_Code> r
            = evaluate_conversion(*initializer_value, type_node.const_value()->get_type());
        if (!r) {
            return Analysis_Error { r.error(), handle, node.get_initializer() };
        }
        node.const_value() = *r;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, ast::Let& node, Analysis_Level level, Expression_Context)
    {
        if (node.get_type() == nullptr) {
            // If there is no type, there must be an initializer.
            // This is "static type inference".
            // Prior analysis should have ensured this already, but we may a well double-check.
            BIT_MANIPULATION_ASSERT(node.get_initializer() != nullptr);

            if (auto r = analyze_types(node.get_initializer(), level, Expression_Context::normal);
                !r) {
                return r;
            }
            auto initializer_value = get_const_value(*node.get_initializer());
            BIT_MANIPULATION_ASSERT(initializer_value.has_value());
            // Intentionally "forget" the value determined during constant folding.
            // For mutable variables, it is possible that the value is modified, and
            // `const_value` should only be a concrete value for invariants.
            node.const_value() = Value::unknown_of_type(initializer_value->get_type());
            return {};
        }

        auto& type_node = std::get<ast::Type>(*node.get_type());
        const auto type_result
            = analyze_types(node.get_type(), type_node, level, Expression_Context::constant);
        if (!type_result) {
            return type_result;
        }
        if (node.get_initializer() == nullptr) {
            node.const_value() = Value::unknown_of_type(type_node.concrete_type().value());
            return {};
        }

        if (const auto r = analyze_types(node.get_initializer(), level, Expression_Context::normal);
            !r) {
            return r;
        }
        auto initializer_value = get_const_value(*node.get_initializer());

        BIT_MANIPULATION_ASSERT(type_node.const_value().has_value());
        if (!initializer_value->get_type().is_convertible_to(type_node.const_value()->get_type())) {
            return Analysis_Error { Analysis_Error_Code::incompatible_types, handle,
                                    node.get_initializer() };
        }
        node.const_value() = Value::unknown_of_type(type_node.concrete_type().value());
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Static_Assert& node,
                                               Analysis_Level,
                                               Expression_Context)
    {
        // Static assertions never need to be checked twice.
        if (node.const_value()) {
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }
        auto expression_result
            = analyze_types(node.get_expression(), Analysis_Level::for_constant_evaluation,
                            Expression_Context::constant);
        if (!expression_result) {
            return expression_result;
        }
        node.const_value() = get_const_value(*node.get_expression());
        BIT_MANIPULATION_ASSERT(node.const_value() && node.const_value()->is_known());

        if (node.const_value()->get_type() != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::static_assert_expression_not_bool, handle,
                                    node.get_expression() };
        }
        if (!node.const_value()->as_bool()) {
            Analysis_Error error { Analysis_Error_Code::static_assertion_failed, handle,
                                   node.get_expression() };
            if (const auto* const comparison
                = std::get_if<ast::Binary_Expression>(node.get_expression());
                comparison && is_comparison_operator(comparison->get_op())) {
                error.comparison_failure = comparison_failure_of(*comparison);
            }
            return error;
        }

        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::If_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        if (auto r = analyze_types(node.get_condition(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition());
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, handle,
                                    node.get_condition() };
        }

        auto& if_block = std::get<ast::Block_Statement>(*node.get_if_block());
        auto if_result
            = analyze_types(node.get_if_block(), if_block, level, Expression_Context::normal);
        if (!if_result) {
            return if_result;
        }
        if (auto else_result
            = analyze_types(node.get_else_block(), level, Expression_Context::normal);
            !else_result) {
            return else_result;
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::While_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        if (auto r = analyze_types(node.get_condition(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition());
        BIT_MANIPULATION_ASSERT(condition_value.has_value());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, handle,
                                    node.get_condition() };
        }

        auto& block = std::get<ast::Block_Statement>(*node.get_block());
        if (auto r = analyze_types(node.get_block(), block, level, Expression_Context::normal);
            !r) {
            return r;
        }
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Break& node, Analysis_Level, Expression_Context)
    {
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Continue& node, Analysis_Level, Expression_Context)
    {
        node.const_value() = Value::Void;
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Return_Statement& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        const Function_Info& function_info = top_function();

        if (!node.get_expression()) {
            if (function_info.return_type != Concrete_Type::Void) {
                return Analysis_Error { Analysis_Error_Code::empty_return_in_non_void_function,
                                        handle, function_info.return_type_node };
            }
            node.const_value() = Concrete_Value::Void;
            return {};
        }

        if (auto r = analyze_types(node.get_expression(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto expr_value = get_const_value(*node.get_expression());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());
        if (!expr_value->get_type().is_convertible_to(function_info.return_type)) {
            return Analysis_Error { Analysis_Error_Code::incompatible_types, handle,
                                    function_info.return_type_node };
        }
        const Result<Value, Conversion_Error_Code> eval_result
            = evaluate_conversion(*expr_value, function_info.return_type);
        node.const_value()
            = eval_result ? *eval_result : Value::unknown_of_type(function_info.return_type);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Assignment& node,
                                               Analysis_Level level,
                                               Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != nullptr);

        ast::Some_Node& looked_up_node = *node.lookup_result;
        if (std::holds_alternative<ast::Parameter>(looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_parameter, handle,
                                    node.lookup_result };
        }
        if (std::holds_alternative<ast::Function>(looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_function, handle,
                                    node.lookup_result };
        }

        if (std::holds_alternative<ast::Const>(looked_up_node)) {
            return Analysis_Error { Analysis_Error_Code::assigning_const, handle,
                                    node.lookup_result };
        }

        auto& looked_up_var = std::get<ast::Let>(looked_up_node);

        if (auto r = analyze_types(node.get_expression(), level, Expression_Context::normal); !r) {
            return r;
        }
        auto expr_value = get_const_value(*node.get_expression());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Concrete_Type dest_type = looked_up_var.const_value().value().get_type();
        if (!expr_value->get_type().is_convertible_to(dest_type)) {
            return Analysis_Error { Analysis_Error_Code::incompatible_types, handle,
                                    node.get_expression() };
        }

        const Result<Value, Conversion_Error_Code> eval_result
            = evaluate_conversion(*expr_value, dest_type);
        if (!eval_result) {
            node.const_value() = Value::unknown_of_type(dest_type);
            return {};
        }
        node.const_value() = *eval_result;
        return {};
    }

    [[gnu::always_inline]] Result<void, Analysis_Error> analyze_types(ast::Some_Node*,
                                                                      ast::Block_Statement& node,
                                                                      Analysis_Level level,
                                                                      Expression_Context)
    {
        return analyze_child_types(node, level, Expression_Context::normal);
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Conversion_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        if (auto r = analyze_types(node.get_expression(), level, context); !r) {
            return r;
        }
        auto& target_type = std::get<ast::Type>(*node.get_target_type());
        if (auto r = analyze_types(node.get_expression(), target_type, level, context); !r) {
            return r;
        }

        const auto& expression_value = get_const_value(*node.get_expression());
        BIT_MANIPULATION_ASSERT(expression_value);

        std::optional<Concrete_Type> type = target_type.concrete_type();
        BIT_MANIPULATION_ASSERT(type);

        if (!expression_value->get_type().is_convertible_to(*type)) {
            return Analysis_Error { Conversion_Error_Code::not_convertible, handle,
                                    node.get_target_type() };
        }

        const Result<Value, Conversion_Error_Code> eval_result
            = evaluate_conversion(*expression_value, *type);

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::If_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        if (auto r = analyze_types(node.get_condition(), level, context); !r) {
            return r;
        }
        auto condition_value = get_const_value(*node.get_condition());
        if (condition_value->get_type() != Concrete_Type::Bool) {
            return Analysis_Error { Analysis_Error_Code::condition_not_bool, handle,
                                    node.get_condition() };
        }
        Expression_Context left_context = context;
        Expression_Context right_context = context;
        // Similar to short-circuiting (see Binary_Expression), we lower the analysis context
        // to `normal` for the expression that is not evaluated.
        // If we analyzed it as a constant expression, we would require a value from it, and
        // e.g. `1 if true else 0 / 0` would fail, even though it is valid and meant to select `1`.
        if (context == Expression_Context::constant) {
            BIT_MANIPULATION_ASSERT(condition_value->is_known());
            const bool evaluate_left = condition_value->as_bool();
            (evaluate_left ? right_context : left_context) = Expression_Context::normal;
        }

        if (auto r = analyze_types(node.get_left(), level, left_context); !r) {
            return r;
        }
        auto left_value = get_const_value(*node.get_left());

        if (auto r = analyze_types(node.get_right(), level, right_context); !r) {
            return r;
        }
        auto right_value = get_const_value(*node.get_right());

        Result<Concrete_Type, Analysis_Error_Code> type_result = check_if_expression(
            left_value->get_type(), condition_value->get_type(), right_value->get_type());
        if (!type_result) {
            return Analysis_Error { type_result.error(), handle };
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_if_expression(*left_value, *condition_value, *right_value);

        // Type analysis has already succeeded, and for context expressions, the condition is known.
        // Therefore, it should be impossible for an evaluation to fail.
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant || eval_result.has_value());

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Binary_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        if (auto r = analyze_types(node.get_left(), level, context); !r) {
            return r;
        }
        const auto& left_value = get_const_value(*node.get_left());
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                || (left_value && left_value->is_known()));

        {
            // For short-circuiting operators like && and ||, if we are in a constant expression,
            // we should not evaluate the rest as a constant expression if short-circuiting takes
            // place.
            //
            // For example, in `const x = true || 0 / 0 == 0;`, the initializer is correct
            // according to type analysis, and the division by zero is never executed.
            // Therefore, only type analysis should be applied upon short-circuiting, not
            // constant expression analysis, which would also demand a value from `0 / 0`.
            const bool is_short_circuiting = node.get_op() == Token_Type::logical_and
                || node.get_op() == Token_Type::logical_or;
            if (context == Expression_Context::constant && is_short_circuiting) {
                if (left_value->get_type() != Concrete_Type::Bool) {
                    return Analysis_Error { Analysis_Error_Code::non_bool_logical, handle,
                                            node.get_left() };
                }
                const bool circuit_breaker = node.get_op() == Token_Type::logical_or;
                if (left_value->as_bool() == circuit_breaker) {
                    context = Expression_Context::normal;
                }
            }
        }

        if (auto r = analyze_types(node.get_right(), level, context); !r) {
            return r;
        }
        const auto& right_value = get_const_value(*node.get_right());
        BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                || (right_value && right_value->is_known()));

        const Result<Concrete_Type, Analysis_Error_Code> type_result
            = check_binary_operator(left_value->get_type(), node.get_op(), right_value->get_type());
        if (!type_result) {
            return Analysis_Error { type_result.error(), handle };
        }

        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_binary_operator(*left_value, node.get_op(), *right_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error { eval_result.error(), handle };
        }

        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Prefix_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        if (auto r = analyze_types(node.get_expression(), level, context); !r) {
            return r;
        }
        const auto expr_value = get_const_value(*node.get_expression());
        BIT_MANIPULATION_ASSERT(expr_value.has_value());

        const Result<Concrete_Type, Analysis_Error_Code> type_result
            = check_unary_operator(node.get_op(), expr_value->get_type());
        if (!type_result) {
            return Analysis_Error { type_result.error(), handle };
        }
        const Result<Value, Evaluation_Error_Code> eval_result
            = evaluate_unary_operator(node.get_op(), *expr_value);
        if (!eval_result && context == Expression_Context::constant) {
            return Analysis_Error { eval_result.error(), handle };
        }
        node.const_value() = eval_result ? *eval_result : Value::unknown_of_type(*type_result);
        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Function_Call_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        BIT_MANIPULATION_ASSERT(node.lookup_result != nullptr);

        // 1. Evaluate arguments.

        std::pmr::monotonic_buffer_resource temp_memory_resource;
        std::pmr::vector<Value> arg_values(&temp_memory_resource);
        arg_values.reserve(node.get_children().size());

        for (ast::Some_Node* const arg : node.get_children()) {
            if (auto r = analyze_types(arg, level, context); !r) {
                return r;
            }
            arg_values.push_back(get_const_value(*arg).value());
        }

        // 2.1. If the function is builtin, evaluate right away.

        if (auto* const builtin = std::get_if<ast::Builtin_Function>(node.lookup_result)) {
            auto type_result = check_builtin_function(builtin->get_function(), arg_values);
            if (!type_result) {
                return Analysis_Error { type_result.error(), handle, node.lookup_result };
            }
            auto eval_result = evaluate_builtin_function(builtin->get_function(), arg_values);
            if (!eval_result) {
                return Analysis_Error { eval_result.error(), handle, node.lookup_result };
            }
            BIT_MANIPULATION_ASSERT(context != Expression_Context::constant
                                    || eval_result.has_value());
            node.const_value() = *eval_result;
            return {};
        }

        // 2.2. Otherwise, verify that we call a function.

        auto* function = std::get_if<ast::Function>(node.lookup_result);
        if (!function) {
            return Analysis_Error { Analysis_Error_Code::call_non_function, handle,
                                    node.lookup_result };
        }

        // 3. Verify that we have the right number of arguments.
        //    We can perform this test prior to deduction because calling a function with the wrong
        //    number of arguments is an error before and after instantiating generic functions.
        //    There is no function overloading.

        const ast::Parameter_List* possibly_generic_params = nullptr;
        if (function->get_parameters() != nullptr) {
            possibly_generic_params = &std::get<ast::Parameter_List>(*function->get_parameters());
            if (possibly_generic_params->get_children().size() != node.get_children().size()) {
                return Analysis_Error { Analysis_Error_Code::wrong_number_of_arguments, handle,
                                        function->get_parameters() };
            }
        }
        else if (node.get_children().size() != 0) {
            return Analysis_Error { Analysis_Error_Code::wrong_number_of_arguments, handle,
                                    node.lookup_result };
        }

        // 4. If necessary, perform bit-generic argument deduction.
        //    After deduction is complete, `function` will point to the concrete instance, not to
        //    the generic function anymore.
        if (function->is_generic) {
            // Functions with no parameters cannot be generic.
            BIT_MANIPULATION_ASSERT(possibly_generic_params != nullptr);
            std::pmr::vector<int> deduced_widths(&temp_memory_resource);
            for (Size i = 0; i < node.get_children().size(); ++i) {
                auto& param = std::get<ast::Parameter>(*possibly_generic_params->get_children()[i]);
                auto& type = std::get<ast::Type>(*param.get_type());
                // If this function is generic, parameter types wouldn't have undergone analysis.
                BIT_MANIPULATION_ASSERT(!type.const_value());
                if (type.concrete_width) {
                    continue;
                }
                auto* gen_expr = std::get_if<ast::Id_Expression>(type.get_width());
                if (!gen_expr || !gen_expr->bit_generic) {
                    continue;
                }
                if (!arg_values[i].get_type().is_uint()) {
                    return Analysis_Error { Analysis_Error_Code::width_deduction_from_non_uint,
                                            node.get_children()[i], type.get_width() };
                }
                deduced_widths.push_back(arg_values[i].get_type().width());
            }

            Result<const ast::Function::Instance*, Analysis_Error> instantiation_result
                = Instantiator { m_program, &m_memory_resource }.instantiate_function(
                    node.lookup_result, *function, deduced_widths);
            if (!instantiation_result) {
                return instantiation_result.error();
            }

            // Using pointers is merely a workaround for Result not working with references.
            // The returned pointer is never null.
            BIT_MANIPULATION_ASSERT(*instantiation_result != nullptr);
            const auto& instance = **instantiation_result;
            function = &std::get<ast::Function>(*instance.handle);
            // Memoization of deduction results.
            // Future analysis of this function call expression will treat it as a call to a
            // concrete instance.
            node.lookup_result = instance.handle;
        }

        // The memoization in the first stage should ensure that we are now looking up the instance.
        // Alternatively, the function wasn't generic in the first place.
        BIT_MANIPULATION_ASSERT(!function->is_generic);

        // 5. Analyze the called concrete function.
        //    If we are in a constant expression, the whole function has to be immediately analyzed
        //    as a constant expression.
        //    Otherwise, we don't need to analyze the function body immediately.

        const auto inner_level = context == Expression_Context::constant
                || level == Analysis_Level::for_constant_evaluation
            ? Analysis_Level::for_constant_evaluation
            : Analysis_Level::shallow;

        // 5.1. It is possible that while analyzing a constant-evaluated function call,
        //      the surrounding function (directly or indirectly) would have to be analyzed.
        //      This is impossible, and we need to detect this case.

        // TODO: instead of a stack, we could also use a bool for functions and constants
        //       or simply all nodes, and track whether they are under analysis
        if (inner_level == Analysis_Level::for_constant_evaluation) {
            for (const Function_Info& info : m_function_stack) {
                if (function == info.function) {
                    // TODO: this should probably use a different diagnostic or the diagnostic
                    //       should be renamed
                    return Analysis_Error { Analysis_Error_Code::codegen_call_to_unanalyzed, handle,
                                            node.lookup_result };
                }
            }
        }

        auto r = analyze_types(node.lookup_result, *function, inner_level, context);
        if (!r) {
            return r;
        }

        BIT_MANIPULATION_ASSERT(function->analysis_so_far >= inner_level);
        Concrete_Type return_type = function->const_value()->get_type();

        // 6. Check whether the function can be called with the given arguments and obtain
        //    concrete values for the parameters if need be.

        const ast::Parameter_List* params = nullptr;
        if (function->get_parameters() != nullptr) {
            params = &std::get<ast::Parameter_List>(*function->get_parameters());
        }

        // 6.1. Make sure that function calls during constant evaluation have been compiled to VM.

        if (context == Expression_Context::constant) {
            BIT_MANIPULATION_ASSERT(function->vm_address != ast::Function::invalid_vm_address);
            constant_evaluation_machine.reset();
            constant_evaluation_machine.jump_to(function->vm_address);
        }

        for (Size i = 0; i < node.get_children().size(); ++i) {
            BIT_MANIPULATION_ASSERT(params != nullptr);
            auto& param = std::get<ast::Parameter>(*params->get_children()[i]);
            const Concrete_Type param_type = param.const_value().value().get_type();
            if (!arg_values[i].get_type().is_convertible_to(param_type)) {
                return Analysis_Error { Analysis_Error_Code::incompatible_types, handle,
                                        params->get_children()[i] };
            }

            Result<Value, Conversion_Error_Code> conv_result
                = evaluate_conversion(arg_values[i], param_type);
            if (context == Expression_Context::constant) {
                if (!conv_result) {
                    return Analysis_Error { conv_result.error(), handle,
                                            params->get_children()[i] };
                }
                constant_evaluation_machine.push(conv_result->concrete_value());
            }
        }

        // 7. Constant-evaluate the function call.

        if (context == Expression_Context::constant) {
            Result<void, Execution_Error> cycle_result;
            while ((cycle_result = constant_evaluation_machine.cycle())) { }
            // Normally, the VM will push and pop function return addresses as frames
            // as part of execution function calls and returns.
            // However, we artificially jump directly to the start of the function without doing so,
            // meaning that there is no return address and we run into an execution error.
            // TODO: this design is questionable; maybe push a magic return address instead
            if (cycle_result.error().code != Execution_Error_Code::pop_call) {
                return Analysis_Error { cycle_result.error(), handle, cycle_result.error().handle };
            }
            BIT_MANIPULATION_ASSERT(constant_evaluation_machine.stack_size() == 1);
            node.const_value() = constant_evaluation_machine.pop();
        }
        else {
            node.const_value() = Value::unknown_of_type(return_type);
        }

        return {};
    }

    Result<void, Analysis_Error> analyze_types(ast::Some_Node* handle,
                                               ast::Id_Expression& node,
                                               Analysis_Level level,
                                               Expression_Context context)
    {
        if (node.const_value()) {
            // In instantiations of bit-generic id-expressions,
            // id-expressions don't turn into literals, but "magically" obtain a const_value.
            // In any other case, a lookup result should exist.
            // Id-expressions never need to be analyzed twice.
            return {};
        }
        BIT_MANIPULATION_ASSERT(node.lookup_result != nullptr);

        if (const auto* const looked_up_function = std::get_if<ast::Function>(node.lookup_result)) {
            return Analysis_Error { Analysis_Error_Code::function_in_expression, handle,
                                    node.lookup_result };
        }
        if (const auto* const looked_up_var = std::get_if<ast::Let>(node.lookup_result)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error { Analysis_Error_Code::let_variable_in_constant_expression,
                                        handle, node.lookup_result };
            }
            if (!looked_up_var->const_value()) {
                return Analysis_Error { Analysis_Error_Code::use_of_undefined_variable, handle,
                                        node.lookup_result };
            }
            node.const_value() = looked_up_var->const_value();
            return {};
        }
        if (const auto* const looked_up_const = std::get_if<ast::Const>(node.lookup_result)) {
            // TODO: consider analyzing it from here, or analyzing global constants
            //       in a separate pass.
            if (!looked_up_const->const_value()) {
                return Analysis_Error { Analysis_Error_Code::use_of_undefined_constant, handle,
                                        node.lookup_result };
            }
            node.const_value() = looked_up_const->const_value();
            return {};
        }
        if (const auto* const looked_up_param = std::get_if<ast::Parameter>(node.lookup_result)) {
            if (context == Expression_Context::constant) {
                return Analysis_Error { Analysis_Error_Code::parameter_in_constant_expression,
                                        handle, node.lookup_result };
            }
        }

        // TODO: is this actually necessary?
        if (auto r = analyze_types(node.lookup_result, level, context); !r) {
            return r;
        }
        const auto lookup_value = get_const_value(*node.lookup_result);
        BIT_MANIPULATION_ASSERT(lookup_value.has_value());
        if (context == Expression_Context::constant && lookup_value->is_unknown()) {
            return Analysis_Error { Analysis_Error_Code::expected_constant_expression, handle,
                                    node.lookup_result };
        }
        node.const_value() = lookup_value;
        return {};
    }

    Result<void, Analysis_Error>
    analyze_types(ast::Some_Node* handle, ast::Literal& node, Analysis_Level, Expression_Context)
    {
        // Literals never need to be analyzed twice.
        if (node.const_value()) {
            BIT_MANIPULATION_ASSERT(node.const_value()->is_known());
            return {};
        }
        switch (node.get_type()) {
        case Token_Type::keyword_true: {
            node.const_value() = Value::True;
            return {};
        }
        case Token_Type::keyword_false: {
            node.const_value() = Value::False;
            return {};
        }
        case Token_Type::binary_literal:
        case Token_Type::octal_literal:
        case Token_Type::decimal_literal:
        case Token_Type::hexadecimal_literal: {
            std::optional<Big_Int> value = parse_integer_literal(node.get_literal());
            if (!value) {
                return Analysis_Error { Analysis_Error_Code::invalid_integer_literal, handle };
            }
            node.const_value() = Value::Int(*value);
            return {};
        }
        default:
            BIT_MANIPULATION_ASSERT_UNREACHABLE("Given token type does not form a valid literal");
        }
    }

    [[gnu::always_inline]] Result<void, Analysis_Error>
    analyze_types(ast::Some_Node*, ast::Builtin_Function& node, Analysis_Level, Expression_Context)
    {
        BIT_MANIPULATION_ASSERT(node.const_value());
        return {};
    }
};

} // namespace

Result<void, Analysis_Error> analyze_semantics(Analyzed_Program& program,
                                               std::pmr::memory_resource* memory)
{
    Type_Analyzer analyzer { program, memory };
    return analyzer();
}

Result<void, Analysis_Error> analyze(Analyzed_Program& program, std::pmr::memory_resource* memory)
{
    std::pmr::unsynchronized_pool_resource local_memory(memory);
    if (auto r = analyze_name_lookup(program, &local_memory); !r) {
        return r;
    }
    local_memory.release();
    return analyze_semantics(program, &local_memory);
}

} // namespace bit_manipulation::bms
