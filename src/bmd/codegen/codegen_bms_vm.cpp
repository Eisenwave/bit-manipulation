#include <unordered_map>

#include "common/code_string.hpp"

#include "bms/analyzed_program.hpp"
#include "bms/ast.hpp"
#include "bms/vm/instructions.hpp"
#include "bms/vm/vm.hpp"

#include "bmd/codegen/codegen.hpp"

namespace bit_manipulation::bmd {
namespace {

std::pmr::unordered_map<Size, const bms::ast::Function*>
collect_function_addresses(std::pmr::memory_resource* memory, const bms::Analyzed_Program& program)
{
    std::pmr::unordered_map<Size, const bms::ast::Function*> result { memory };

    const auto emit = [&](const bms::ast::Function& f) {
        if (const std::optional<Size> address = f.get_vm_address()) {
            result.emplace(*address, &f);
        }
    };

    const auto& program_node = get<bms::ast::Program>(*program.get_root());
    for (const bms::ast::Some_Node* declaration : program_node.get_children()) {
        const bms::ast::Function* function = get_if<bms::ast::Function>(declaration);
        if (!function) {
            continue;
        }
        emit(*function);
        for (const bms::ast::Function::Instance& instance : function->instances) {
            emit(instance.get_function());
        }
    }

    return result;
}

} // namespace

[[nodiscard]] Result<void, Generator_Error>
generate_bms_vm_code(Code_String& out,
                     const bms::Analyzed_Program& program,
                     std::pmr::memory_resource* memory,
                     const Code_Options&)
{
    const bms::Virtual_Machine& vm = program.get_vm();

    std::pmr::unordered_map<Size, const bms::ast::Function*> function_labels
        = collect_function_addresses(memory, program);

    bms::print_program(out, vm.instructions(), [&](Code_String& out, Size index) -> bool { //
        auto it = function_labels.find(index);
        if (it == function_labels.end()) {
            return false;
        }
        bms::print_function_label(out, *it->second,
                                  { .parameters = true, .return_type = true, .whitespace = false });
        return true;
    });

    return {};
}

} // namespace bit_manipulation::bmd
