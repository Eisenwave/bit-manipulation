#include "common/variant.hpp"

#include "bmd/codegen/declarations.hpp"

namespace bit_manipulation::bmd {
namespace {

[[nodiscard]] Dependency_Type dependency_type_constant_recursive(bool constant, bool recursive)
{
    return constant
        ? (recursive ? Dependency_Type::constant_recursive : Dependency_Type::constant_direct)
        : (recursive ? Dependency_Type::normal_recursive : Dependency_Type::normal_direct);
}

struct Dependency_Gatherer {
    Function_Ref<bool(Dependency)>& out;
    bool constant : 1 = false;
    bool recursive : 1 = false;

    void operator()(const bms::ast::Program&)
    {
        BIT_MANIPULATION_ASSERT_UNREACHABLE("Unexpected AST structure.");
    }

    void operator()(const bms::ast::Function& f)
    {
        for (const auto& p : f.get_parameters()) {
            (*this)(p.get_type());
        }
        if (f.get_return_type_node()) {
            (*this)(f.get_return_type());
        }
        if (const auto* requires_clause = f.get_requires_clause_node()) {
            visit(in_constant_expression(true), *requires_clause);
        }
        in_constant_expression(false)(f.get_body());
    }

    template <one_of<bms::ast::Let,
                     bms::ast::If_Statement,
                     bms::ast::While_Statement,
                     bms::ast::Control_Statement,
                     bms::ast::Assignment,
                     bms::ast::Block_Statement,
                     bms::ast::Conversion_Expression,
                     bms::ast::If_Expression,
                     bms::ast::Binary_Expression,
                     bms::ast::Prefix_Expression> T>
    void operator()(const T& block)
    {
        for (const bms::ast::Some_Node* child : block.get_children()) {
            if (child != nullptr) {
                visit(in_constant_expression(false), *child);
            }
        }
    }

    template <one_of<bms::ast::Type, bms::ast::Const, bms::ast::Static_Assert> T>
    void operator()(const T& block)
    {
        for (const bms::ast::Some_Node* child : block.get_children()) {
            if (child != nullptr) {
                visit(in_constant_expression(true), *child);
            }
        }
    }

    template <one_of<bms::ast::Function_Call_Expression, bms::ast::Id_Expression> T>
    void operator()(const T& node)
    {
        emit(node.lookup_result);
    }

    void operator()(const bms::ast::Literal&)
    {
        // literals have no dependencies
    }

private:
    /// @brief Returns a copy of this `Dependency_Gatherer`, except that `constant` is set to
    /// `constant_expression` in the new object.
    /// This function should be used whenever entering or leaving a context that is a constant
    /// expression.
    [[nodiscard]] Dependency_Gatherer in_constant_expression(bool constant_expression)
    {
        return { .out = out, .constant = constant_expression, .recursive = recursive };
    }

    void emit(const bms::Optional_Lookup_Result& lookup_result)
    {
        if (const auto* const* looked_up_node = get_if<bms::ast::Some_Node*>(&lookup_result)) {
            if (const auto* const c = get_if<bms::ast::Const>(*looked_up_node)) {
                if (!holds_alternative<bms::ast::Program>(*c->get_parent())) {
                    return;
                }
                emit(**looked_up_node, *c);
            }
            else {
                const auto& f = get<bms::ast::Function>(**looked_up_node);
                emit(**looked_up_node, f);
            }
            return;
        }
        if (const auto* const* looked_up_parameter = get_if<bms::Parameter*>(&lookup_result)) {
            (*this)((*looked_up_parameter)->get_type());
            return;
        }
        BIT_MANIPULATION_ASSERT(holds_alternative<bms::Builtin_Function>(lookup_result));
    }

    template <one_of<bms::ast::Function, bms::ast::Const> T>
    void emit(const bms::ast::Some_Node& some_node, const T& node)
    {
        const Dependency_Type type = dependency_type_constant_recursive(constant, recursive);
        const bool should_recurse = out({ &some_node, type });
        if (should_recurse) {
            Dependency_Gatherer { .out = out, .constant = constant, .recursive = true }(node);
        }
    }
};

} // namespace

void for_each_global_dependency(Function_Ref<bool(Dependency)> out, const bms::ast::Some_Node& node)
{
    visit(Dependency_Gatherer { out }, node);
}

void for_each_direct_global_dependency(Function_Ref<void(Dependency)> out,
                                       const bms::ast::Some_Node& node)
{
    for_each_global_dependency(
        [&](Dependency d) -> bool {
            out(d);
            return false;
        },
        node);
}

namespace {

using Graph_Index = Size;
using Index_Vector = std::pmr::vector<Graph_Index>;

/// @brief Single-use, callable type which implements `break_dependencies`.
/// The implementation is based on:
/// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
///
/// The intuition is relatively simple:
/// - If we perform depth-first search by following dependencies of a vertex, we may eventually
///   reach a leaf node, and this can be emitted because it depends on nothing more.
/// - Alternatively, we don't find a leaf node but a strongly connected component (SCC),
///   which is a subgraph where every node is reachable from the other.
///   In other words, some sort of circular dependency spaghetti block, and we need to emit a
///   forward-declaration in order to break it up.
///
/// Tarjan's algorithm specializes in finding such SCC.
/// We slightly adapt it so that the forward declarations emitted are for those entities that are
/// defined last in the program.
struct [[nodiscard]] Dependency_Breaker {
private:
    struct [[nodiscard]] Vertex_Data {
        // TODO: it's probably possible to store this data contiguously elsewhere and replace this
        //       with an index/pointer into that larger block
        Index_Vector dependencies;
        Graph_Index lowlink = Graph_Index(-1);
        /// @brief `true` if the node has been visited by DFS already.
        bool visited : 1 = false;
        /// @brief `true` if the node is currently being visited by DFS on any depth level.
        /// This also means that it is somewhere in `visit_stack`.
        bool onstack : 1 = false;
        /// @brief `true` if the node has been forward-declared, which can be considered as no
        /// node having a dependency on it anymore.
        bool declared : 1 = false;

        explicit Vertex_Data(std::pmr::memory_resource* memory)
            : dependencies { memory }
        {
        }
    };

    Function_Ref<void(Size, bool)> m_out;
    const Graph_Index m_n;

    std::pmr::monotonic_buffer_resource resource;

    std::pmr::vector<Vertex_Data> data { m_n, Vertex_Data { &resource }, &resource };

    Index_Vector next_to_dfs { &resource };
    Index_Vector visit_stack { &resource };
    Graph_Index visit_counter = 0;

public:
    explicit Dependency_Breaker(Function_Ref<void(Size, bool)>& out,
                                Graph_Index n,
                                std::span<const Edge> edges,
                                std::pmr::memory_resource* memory)
        : m_out { out }
        , m_n { n }
        , resource { memory }
    {
        visit_stack.reserve(m_n * 2);
        next_to_dfs.reserve(m_n * 2);

        for (const Edge e : edges) {
            BIT_MANIPULATION_ASSERT(e.from < m_n);
            BIT_MANIPULATION_ASSERT(e.to < m_n);
            // Circular edges indicate direct recursion within functions,
            // and we should filter those out because they would make the algorithm harder
            // to implement.
            if (e.from != e.to) {
                data[e.from].dependencies.push_back(e.to);
            }
        }
    }

    /// @brief Runs the dependency breaking algorithm.
    /// This function shall not be called more than once.
    void operator()()
    {
        // We push in reverse order so that the nodes are visited in the original order when
        // treating `next_to_dfs` as a stack below.
        for (Graph_Index i = m_n; i-- != 0;) {
            next_to_dfs.push_back(i);
        }

        while (!next_to_dfs.empty()) {
            const Graph_Index current = next_to_dfs.back();
            next_to_dfs.pop_back();
            BIT_MANIPULATION_ASSERT(current < m_n);
            if (!data[current].visited) {
                dfs(current);
            }
        }
    }

private:
    void dfs(const Graph_Index current)
    {
        BIT_MANIPULATION_ASSERT(current < m_n);
        auto& curr_data = data[current];

        const Graph_Index curr_counter = visit_counter;

        curr_data.visited = true;
        curr_data.onstack = true;
        curr_data.lowlink = visit_counter;

        visit_stack.push_back(current);
        visit_counter += 1;

        for (const Graph_Index dependency : curr_data.dependencies) {
            auto& dependency_data = data[dependency];
            if (dependency_data.declared) {
                continue;
            }

            const bool should_visit = !dependency_data.visited;
            if (should_visit) {
                dfs(dependency);
            }
            // TODO: if we've just visited the dependency, onstack should be true for it anyway
            //       so maybe this condition could be simplified to just (dependency_data.onstack)
            if (should_visit || dependency_data.onstack) {
                curr_data.lowlink = std::min(curr_data.lowlink, dependency_data.lowlink);
            }
        }

        // Base case.
        // Note that lowlink indicates which node in the SCC is a "root node",
        // which is the first node we've encountered during DFS that is part of this SCC.
        // Once the algorithm reaches this point, where we've visited dependencies and reach the
        // root node again, this indicates that we've found an SCC and are ready to emit it.
        if (curr_data.lowlink != curr_counter) {
            return;
        }

        // Another base case of the recursive algorithm.
        // If we reach this point, this means that we have an SCC of size 1, or a leaf node in other
        // words that is not strongly connected to any other node.
        // No forward declaration is necessary and we can simply emit a definition.
        //
        // Note that because we pushed the original set of nodes so that they're visited in
        // declaration order (i.e. we visit them like 0, 1, 2, ...) we can also be certain that
        // this node is not a forward-dependency of any previous definition.
        // After all, we would have performed DFS and reached the current node at some point
        // if that was the case.
        if (visit_stack.back() == current) {
            emit_definition(current);
            visit_stack.pop_back();
            curr_data.onstack = false;
            return;
        }

        // Reaching this point means that there is a multi-node SCC, i.e. some kind of circular
        // dependency.

        Graph_Index last_in_scc = 0;
        const Graph_Index curr_pos = stack_pos_of(current);
        const Graph_Index ns = next_to_dfs.size();
        for (Graph_Index i = curr_pos; i < visit_stack.size(); ++i) {
            // TODO: investigate if this is actually a meaningful order to push; in past examples
            //       it's not resulted in scrambling but hey, maybe I've missed something
            //       examples such as
            //       https://discord.com/channels/331718482485837825/893852731192782898/1335342809028366387
            //       may break this
            next_to_dfs.push_back(visit_stack[i]);
            last_in_scc = std::max(last_in_scc, visit_stack[i]);
        }

        emit_declaration(last_in_scc);
        data[last_in_scc].declared = true;
        pop_strongly_connected_component(current, curr_pos);

        // TODO: this is very similar to the main loop (spiritually, it operates with a constant
        //       k = ns while the main loop operates with k = 0)
        //       we could eliminate that redundancy
        while (next_to_dfs.size() != ns) {
            const Graph_Index next = next_to_dfs.back();
            next_to_dfs.pop_back();
            if (!data[next].visited) {
                dfs(next);
            }
        }
    }

    void pop_strongly_connected_component(const Graph_Index node, const Graph_Index node_stack_pos)
    {
        BIT_MANIPULATION_ASSERT(node < m_n);
        const Graph_Index root_lowlink = data[node].lowlink;

        // TODO: we can presumably get rid of this member function and inline it into DFS,
        //       while also doing next_to_dfs.push_back(visit_stack.at(i)) in this very loop
        for (Graph_Index i = node_stack_pos; i < visit_stack.size(); ++i) {
            const Graph_Index removed_node = visit_stack[i];
            auto& removed_data = data[removed_node];
            BIT_MANIPULATION_ASSERT(removed_data.lowlink >= root_lowlink);
            removed_data.visited = false;
            removed_data.onstack = false;
            removed_data.lowlink = Graph_Index(-1);
        }

        visit_stack.resize(node_stack_pos);
    }

    /// @brief Returns the index of the given `node` within `visit_stack`.
    [[nodiscard]] Graph_Index stack_pos_of(const Graph_Index node) const
    {
        BIT_MANIPULATION_ASSERT(node < m_n);
        Graph_Index curr_pos = Graph_Index(visit_stack.size()) - 1;
        while (visit_stack[curr_pos] != node) {
            curr_pos -= 1;
        }
        return curr_pos;
    }

    void emit_definition(const Graph_Index node) const
    {
        BIT_MANIPULATION_ASSERT(node < m_n);
        m_out(node, false);
    }

    void emit_declaration(const Graph_Index node) const
    {
        BIT_MANIPULATION_ASSERT(node < m_n);
        m_out(node, true);
    }
};

} // namespace

void break_dependencies(Function_Ref<void(Size, bool)> out,
                        Size n,
                        std::span<const Edge> dependencies,
                        std::pmr::memory_resource* memory)
{
    Dependency_Breaker(out, n, dependencies, memory)();
}

} // namespace bit_manipulation::bmd
