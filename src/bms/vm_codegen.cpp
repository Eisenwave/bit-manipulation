#include "bms/analyze.hpp"
#include "bms/vm.hpp"

using namespace bit_manipulation::bms::ast;

namespace bit_manipulation::bms {

struct Virtual_Code_Generator : Analyzer_Base {
    Virtual_Code_Generator(Parsed_Program& program)
        : Analyzer_Base(program)
    {
    }

public:
    Analysis_Result operator()(Function_Node& function)
    {
        return generate_code(function);
    }

private:
    Analysis_Result generate_code(Node_Handle h)
    {
        return std::visit([this](auto& node) { return generate_code(node); }, get_node(h));
    }

    template <typename T>
    Analysis_Result generate_code(T& node)
    {
        for (Node_Handle child : node.get_children()) {
            if (auto r = generate_code(child); !r) {
                return r;
            }
        }
        return Analysis_Result::ok;
    }

    template <>
    Analysis_Result generate_code(Program_Node&)
    {
        BIT_MANIPULATION_ASSERT(false);
    }
};

} // namespace bit_manipulation::bms