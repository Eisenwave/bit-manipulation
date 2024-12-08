#include <bitset>

#include "common/packs.hpp"
#include "common/parse.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/annotation.hpp"
#include "bms/ast.hpp"
#include "bms/parsing/astp.hpp"
#include "bms/parsing/parse.hpp"

namespace bit_manipulation::bms {

namespace {

struct Annotation_Parameter {
    Annotation_Parameter_Type type;
    std::string_view name;
    bool variadic : 1 = false;
    bool optional : 1 = false;
};

[[nodiscard]] constexpr Annotation_Parameter_Type parameter_type_of(Token_Type type)
{
    using enum Token_Type;
    switch (type) {
    case keyword_true:
    case keyword_false: return Annotation_Parameter_Type::boolean;

    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: return Annotation_Parameter_Type::integer;

    case string_literal: return Annotation_Parameter_Type::string;

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("No matching parameter type.");
    }
}

[[nodiscard]] constexpr bool parameter_accepts(Annotation_Parameter_Type parameter,
                                               Token_Type argument)
{
    return parameter_type_of(argument) == parameter;
}

[[nodiscard]] constexpr bool annotation_type_applicable_to(Annotation_Type type,
                                                           Construct construct)
{
    using enum Annotation_Type;

    switch (type) {
    case immutable: //
        return construct == Construct::variable;
    case inline_:
        return construct == Construct::variable || construct == Construct::function_call_expression;
    case loop_variable: //
        return construct == Construct::variable;
    case loop_step: //
        return construct == Construct::assignment;
    case unroll: //
        return construct == Construct::while_statement;
    case c_equivalent: //
        return construct == Construct::function;
    case java_equivalent: //
        return construct == Construct::function;
    case instantiate: //
        return construct == Construct::function;
    case false_if_unknown: //
        return construct == Construct::if_statement;
    case remove_if_unused: //
        return construct == Construct::function;
    }
    return false;
}

constexpr Annotation_Parameter unroll_parameters[] = {
    { .type = Annotation_Parameter_Type::integer, .name = "limit" },
};

constexpr Annotation_Parameter c_equivalent_parameters[] = {
    { .type = Annotation_Parameter_Type::string, .name = "function" },
    { .type = Annotation_Parameter_Type::string, .name = "include", .optional = true },
    { .type = Annotation_Parameter_Type::integer, .name = "since", .optional = true },
};

constexpr Annotation_Parameter java_equivalent_parameters[] = {
    { .type = Annotation_Parameter_Type::string, .name = "method" },
};

constexpr Annotation_Parameter instantiate_parameters[] = {
    { .type = Annotation_Parameter_Type::integer, .name = "n" },
};

[[nodiscard]] std::span<const Annotation_Parameter> parameters_of(Annotation_Type type)
{
    using enum Annotation_Type;
    switch (type) {
    case immutable:
    case inline_:
    case loop_variable:
    case loop_step:
    case false_if_unknown:
    case remove_if_unused: return {};

    case unroll: return unroll_parameters;
    case c_equivalent: return c_equivalent_parameters;
    case java_equivalent: return java_equivalent_parameters;
    case instantiate: return instantiate_parameters;
    }
    BIT_MANIPULATION_ASSERT_UNREACHABLE("Unknown annotation type.");
}

[[nodiscard]] Debug_Info argument_debug_info(const astp::Annotation_Argument& argument,
                                             std::string_view file_name)
{
    Source_Position pos { argument.pos, file_name };
    return { Construct::annotation_argument, pos, argument.key };
};

[[nodiscard]] Result<Annotation_Argument, Analysis_Error>
parse_argument(const astp::Annotation_Argument& argument, std::string_view file_name)
{
    BIT_MANIPULATION_ASSERT(is_literal(argument.value_type));

    using enum Token_Type;
    switch (argument.value_type) {
    case keyword_false: return Annotation_Argument { false };
    case keyword_true: return Annotation_Argument { true };

    case binary_literal:
    case octal_literal:
    case decimal_literal:
    case hexadecimal_literal: {
        std::optional<Big_Int> value = parse_integer_literal(argument.value);
        if (!value) {
            return Analysis_Error_Builder { Analysis_Error_Code::invalid_integer_literal }
                .fail(argument_debug_info(argument, file_name))
                .build();
        }
        return Annotation_Argument { *value };
    }

    case string_literal: {
        BIT_MANIPULATION_ASSERT(!argument.value.starts_with('"') && !argument.value.ends_with('"'));
        return Annotation_Argument { argument.value };
    }

    default: BIT_MANIPULATION_ASSERT_UNREACHABLE("Invalid argument type.");
    }
}

struct Annotation_Parameter_Matcher {
    std::bitset<10> parameters_covered;
    const Debug_Info& annotation_info;
    const std::span<const Annotation_Parameter> parameters;

    [[nodiscard]] Annotation_Parameter_Matcher(const Debug_Info& annotation_info,
                                               Annotation_Type type)
        : annotation_info { annotation_info }
        , parameters { parameters_of(type) }
    {
        BIT_MANIPULATION_ASSERT(parameters.size() <= parameters_covered.size());
    }

    [[nodiscard]] Result<Annotation_Parameter, Analysis_Error>
    operator()(const astp::Annotation_Argument& argument)
    {
        return argument.key.empty() ? match_positional(argument) : match_named(argument);
    }

    [[nodiscard]] Result<void, Analysis_Error> check_for_missing() const
    {
        for (Size i = 0; i < parameters.size(); ++i) {
            if (!parameters[i].optional && !parameters_covered[i]) {
                return Analysis_Error_Builder { Analysis_Error_Code::annotation_missing_argument }
                    .fail(annotation_info)
                    .cause(make_debug_info(parameters[i]))
                    .build();
            }
        }
        return {};
    }

private:
    [[nodiscard]] Result<Annotation_Parameter, Analysis_Error>
    match_positional(const astp::Annotation_Argument& argument)
    {
        for (Size i = 0; i < parameters.size(); ++i) {
            if (!parameters_covered[i]) {
                parameters_covered.set(i);
                return parameters[i];
            }
        }
        return error(Analysis_Error_Code::annotation_too_many_arguments, argument);
    }

    [[nodiscard]] Result<Annotation_Parameter, Analysis_Error>
    match_named(const astp::Annotation_Argument& argument)
    {
        for (Size i = 0; i < parameters.size(); ++i) {
            if (parameters[i].name != argument.key) {
                continue;
            }
            if (parameters_covered[i]) {
                return error(Analysis_Error_Code::annotation_duplicate_argument, argument);
            }
            parameters_covered.set(i);
            return parameters[i];
        }
        return error(Analysis_Error_Code::annotation_unknown_parameter, argument);
    }

    [[nodiscard]] Analysis_Error error(Analysis_Error_Code code,
                                       const astp::Annotation_Argument& argument) const
    {
        return Analysis_Error_Builder { Analysis_Error_Code::annotation_unknown_parameter }
            .fail(make_debug_info(argument))
            .cause(annotation_info)
            .build();
    }

    [[nodiscard]] Debug_Info make_debug_info(const astp::Annotation_Argument& argument) const
    {
        return argument_debug_info(argument, annotation_info.pos->file_name);
    }

    [[nodiscard]] Debug_Info make_debug_info(const Annotation_Parameter& parameter) const
    {
        return { Construct::annotation_parameter, {}, parameter.name };
    }
};

} // namespace

struct Resolve_Annotations {
    const Parsed_Program& m_parsed;
    Analyzed_Program& m_program;
    ast::Some_Node* m_node;

public:
    template <typename T>
    Result<void, Analysis_Error> operator()(T& n)
    {
        static_assert(!std::is_base_of_v<ast::detail::Annotated, T>);
        return resolve_all(n.get_children());
    }

    // TODO: a lot of these implementations could be merged if we has bms::ast::get_construct
    template <one_of<ast::Const, ast::Let, ast::While_Statement, ast::If_Statement> T>
    Result<void, Analysis_Error> operator()(ast::Const& n)
    {
        do_resolve(m_node, n.m_annotations);
        return resolve_all(n.get_children());
    }

    Result<void, Analysis_Error> operator()(ast::Function& n)
    {
        if (auto r = do_resolve(m_node, n.m_annotations); !r) {
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

    Result<void, Analysis_Error> do_resolve(const Debug_Info& applied_to_info,
                                            ast::detail::Annotations& annotations)
    {
        // FIXME: this not robust against synthetic AST nodes
        const std::string_view file_name = applied_to_info.pos->file_name;

        std::pmr::vector<Annotation> result { m_program.get_memory_resource() };
        result.reserve(annotations.m_parsed.size());

        for (astp::Handle handle : annotations.m_parsed) {
            const auto& annotation = get<astp::Annotation>(m_parsed.get_node(handle));
            std::optional<Annotation_Type> type = annotation_type_by_name(annotation.name);

            const Debug_Info info { Construct::annotation,
                                    Source_Position { annotation.pos, file_name },
                                    annotation.name };

            if (!type) {
                return Analysis_Error_Builder { Analysis_Error_Code::annotation_unknown }
                    .fail(info)
                    .cause(applied_to_info)
                    .build();
            }

            if (annotation_type_applicable_to(*type, applied_to_info.construct)) {
                return Analysis_Error_Builder { Analysis_Error_Code::annotation_not_applicable }
                    .fail(info)
                    .cause(applied_to_info)
                    .build();
            }
            Result<std::pmr::vector<Annotation_Argument>, Analysis_Error> arguments
                = process_arguments(info, *type, annotation.arguments);
            if (!arguments) {
                return arguments.error();
            }

            result.push_back(Annotation { *type, std::move(*arguments) });
        }
        annotations.m_concrete = std::move(result);

        return {};
    }

    Result<std::pmr::vector<Annotation_Argument>, Analysis_Error>
    process_arguments(const Debug_Info& annotation_info,
                      Annotation_Type type,
                      std::span<const astp::Handle> arguments)
    {
        if (type == Annotation_Type::instantiate && arguments.empty()) {
            return Analysis_Error_Builder { Analysis_Error_Code::annotation_missing_argument }
                .fail(annotation_info)
                .build();
        }
        const std::string_view file_name = annotation_info.pos->file_name;

        std::pmr::vector<Annotation_Argument> result { m_program.get_memory_resource() };
        result.reserve(arguments.size());

        Annotation_Parameter_Matcher matcher { annotation_info, type };

        for (Size i = 0; i < arguments.size(); ++i) {
            const auto& argument = get<astp::Annotation_Argument>(m_parsed.get_node(arguments[i]));
            Result<Annotation_Parameter, Analysis_Error> parameter = matcher(argument);
            if (!parameter) {
                return parameter.error();
            }
            if (!parameter_accepts(parameter->type, argument.value_type)) {
                return Analysis_Error_Builder {
                    Analysis_Error_Code::annotation_wrong_argument_type
                }
                    .fail(argument_debug_info(argument, file_name))
                    .wrong_argument({ parameter->name, parameter->type,
                                      parameter_type_of(argument.value_type) })
                    .cause(annotation_info)
                    .build();
            }

            Result<Annotation_Argument, Analysis_Error> parsed
                = parse_argument(argument, file_name);
            if (!parsed) {
                return parsed.error();
            }
            result.push_back(std::move(*parsed));
        }

        if (auto r = matcher.check_for_missing(); !r) {
            return r.error();
        }

        return result;
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

Result<void, Analysis_Error> resolve_annotations(Analyzed_Program& program,
                                                 const Parsed_Program& parsed)
{
    ast::Some_Node* root = program.get_root();
    return Resolve_Annotations { parsed, program, root }(get<ast::Program>(*root));
}

} // namespace bit_manipulation::bms
