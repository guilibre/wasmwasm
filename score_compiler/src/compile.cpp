#include "compile.hpp"

#include "backend/codegen.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"

auto compile_to_json(const std::string &source) -> std::string {
    Tokenizer tokenizer(source);
    Parser parser(tokenizer);
    Program program = parser.parse();
    ExpandedGraph graph = expand_program(program);
    return graph_to_json(graph);
}
