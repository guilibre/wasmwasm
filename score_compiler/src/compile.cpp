#include "compile.hpp"

#include "backend/codegen.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"
#include <sstream>

auto compile_to_typescript(const std::string &source) -> std::string {
    const Tokenizer tokenizer(source);
    Parser parser(tokenizer);
    auto program = parser.parse();
    auto resolved = resolve_program(program);

    std::ostringstream out;
    CodeGenerator{}.generate(resolved, out);
    return out.str();
}
