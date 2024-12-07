#include "bms/analyzed_program.hpp"
#include "bms/annotation.hpp"
#include "bms/ast.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/parse.hpp"

namespace bit_manipulation::bms {

struct Resolve_Annotations {
    const Parsed_Program& m_parsed;
    Analyzed_Program& m_program;
    ast::Some_Node* m_node;

private:
    Result<void, Analysis_Error> resolve(ast::Some_Node* node)
    {
        if (node == nullptr) {
            return {};
        }
        return visit(Resolve_Annotations { m_parsed, m_program, node }, *node);
    }

    Result<void, Analysis_Error> resolve_all(std::span<ast::Some_Node* const> nodes)
    {
        for (ast::Some_Node* child : nodes) {
            auto r = resolve(child);
            if (!r) {
                return r;
            }
        }
        return {};
    }

public:
    template <typename T>
    Result<void, Analysis_Error> operator()(T& n)
    {
        static_assert(!std::is_base_of_v<ast::detail::Annotated, T>);
        return resolve_all(n.get_children());
    }

    // TODO: a lot of these implementations could be merged if we has bms::ast::get_construct
    Result<void, Analysis_Error> operator()(ast::Const& n)
    {
        do_resolve(m_node, n.m_annotations, Construct::constant);
        return resolve_all(n.get_children());
    }

    Result<void, Analysis_Error> operator()(ast::Let& n)
    {
        do_resolve(m_node, n.m_annotations, Construct::variable);
        return resolve_all(n.get_children());
    }

#if 0 // TODO: add annotations to these nodes grammar
    Result<void, Analysis_Error> operator()(ast::While_Statement& n)
    {
        do_resolve(m_node, n.m_annotations, Construct::variable);
        return resolve_all(n.get_children());
    }

    Result<void, Analysis_Error> operator()(ast::If_Statement& n)
    {
        do_resolve(m_node, n.m_annotations, Construct::variable);
        return resolve_all(n.get_children());
    }
#endif

    Result<void, Analysis_Error> operator()(ast::Function& n)
    {
        if (auto r = do_resolve(m_node, n.m_annotations, Construct::function); !r) {
            return r;
        }
        for (Parameter& p : n.get_parameters()) {
            if (auto r = resolve(p.get_type_node()); !r) {
                return r;
            }
        }
        if (auto r = resolve(n.get_return_type_node()); !r) {
            return r;
        }
        if (auto r = resolve(n.get_requires_clause_node()); !r) {
            return r;
        }
        if (auto r = resolve(n.get_body_node()); !r) {
            return r;
        }
        return {};
    }

    Result<void, Analysis_Error> do_resolve(ast::Some_Node* applied_to_node,
                                            ast::detail::Annotations& annotations,
                                            Construct applied_to_construct)
    {
        // FIXME: this is hacky and not robust against synthetic AST nodes
        const std::string_view file_name
            = bms::ast::get_source_position(*applied_to_node).value().file_name;

        std::pmr::vector<Annotation> result { m_program.get_memory_resource() };
        result.reserve(annotations.m_parsed.size());

        for (astp::Handle handle : annotations.m_parsed) {
            const auto& annotation = get<astp::Annotation>(m_parsed.get_node(handle));
            std::optional<Annotation_Type> type = annotation_type_by_name(annotation.name);

            const auto make_debug_info = [&]() -> Debug_Info {
                return { Construct::annotation, Source_Position { annotation.pos, file_name },
                         annotation.name };
            };

            if (!type) {
                return Analysis_Error_Builder { Analysis_Error_Code::annotation_unknown }
                    .fail(make_debug_info())
                    .cause(applied_to_node)
                    .build();
            }

            if (annotation_type_applicable_to(*type, applied_to_construct)) {
                return Analysis_Error_Builder { Analysis_Error_Code::annotation_not_applicable }
                    .fail(make_debug_info())
                    .cause(applied_to_node)
                    .build();
            }

            // FIXME: parse arguments
            result.push_back(Annotation { *type, {} });
        }
        annotations.m_concrete = std::move(result);

        return {};
    }
};

#if 0
struct Annotation_Verifier {
private:
    Analyzed_Program& m_program;
    ast::Some_Node* handle;

public:
    Annotation_Verifier(Analyzed_Program& program)
        : m_program(program)
    {
    }

    Result<void, Analysis_Error> operator()()
    {
        auto& program = get<ast::Program>(*m_program.get_root());

        auto first_result = run_pass<Register_Global_Declarations>(program);
        if (!first_result) {
            return first_result;
        }

        return run_pass<Analyze_Symbols_Global>(get<ast::Program>(*m_program.get_root()));
    }

private:
    Result<void, Analysis_Error> operator()(ast::Const& n)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);
        auto it_or_handle = self.m_symbols.emplace(n.get_name(), handle, shadowing);
        if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
            return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_global_const }
                .fail(handle)
                .cause(*old)
                .build();
        }
        return {};
    }

    Result<void, Analysis_Error> operator()(ast::Function& n)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);
        auto it_or_handle = self.m_symbols.emplace(n.get_name(), handle, shadowing);
        if (auto* old = get_if<Lookup_Result>(&it_or_handle)) {
            return Analysis_Error_Builder { Analysis_Error_Code::failed_to_define_function }
                .fail(handle)
                .cause(*old)
                .build();
        }
        return {};
    }

    Result<void, Analysis_Error> operator()(ast::Static_Assert&)
    {
        BIT_MANIPULATION_ASSERT(handle != nullptr);
        return {};
    }

    Result<void, Analysis_Error> operator()(Ignore)
    {
        // nothing to do for nodes that have no annotations
    }
};
#endif

} // namespace bit_manipulation::bms
