#include "compile.hpp"

#include "ast/simplify.hpp"
#include "backend/codegen.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"

auto compile_to_json(const std::string &source) -> std::string {
    const Tokenizer tokenizer(source);
    Parser parser(tokenizer);
    const Program program = simplify_program(parser.parse());
    const ExpandedGraph graph = expand_program(program);
    return graph_to_json(graph);
}
