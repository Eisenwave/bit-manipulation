#include <iostream>

#include "assert.hpp"
#include "bmscript.hpp"

namespace bit_manipulation {

int main()
{
    std::string_view program = R"_(function  awoo(i: Uint(32)) -> Uint(32) {
      /* block comment */  return i;
       // inline comment
    })_";

    std::vector<Token> tokens;
    if (const auto result = tokenize(tokens, program)) {
        for (const auto& t : tokens) {
            const std::string_view text = t.extract(program);
            std::cout << t.pos.line + 1 << ":" << t.pos.column + 1 << ": "
                      << token_type_name(t.type) << "(" << text << ")";
            if (text.length() > 1) {
                std::cout << " (" << text.length() << " characters)";
            }
            std::cout << '\n';
        }
    }
    else {
        std::cout << "Failed to tokenize\n";
    }

    return 0;
}

} // namespace bit_manipulation

int main()
{
    return bit_manipulation::main();
}