#include "code_gen.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"
#include "types/type.hpp"
#include "types/type_inference.hpp"

#include <iostream>
#include <unordered_map>
#include <variant>

extern "C" auto run_compiler(float sample_freq) -> int {
    return code_gen::test(sample_freq);
}

namespace {

void print_type(const TypePtr &type) {
    std::visit(
        [&](const auto &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeVar>) {
                std::cout << "t" << node.id;
            } else if constexpr (std::is_same_v<T, TypeBase>) {
                switch (node.kind) {
                case BaseTypeKind::Int:
                    std::cout << "Int";
                    break;
                case BaseTypeKind::Float:
                    std::cout << "Float";
                    break;
                case BaseTypeKind::Bool:
                    std::cout << "Bool";
                    break;
                }
            } else if constexpr (std::is_same_v<T, TypeFun>) {
                std::cout << "(";
                print_type(node.param);
                std::cout << " -> ";
                print_type(node.result);
                std::cout << ")";
            }
        },
        type->node);
}

} // namespace

auto main() -> int {
    std::string src = "2 * 3 -> x\nx -> OUT";

    try {
        Tokenizer tokenizer(src);
        Parser parser(tokenizer);
        auto expr = parser.parse_expr();

        std::unordered_map<std::string_view, TypePtr> env;
        Substitution subst;

        TypePtr inferred_type = infer_expr(expr, env, subst);

        std::cout << "Inferred type: ";
        print_type(inferred_type);
        std::cout << "\n";

    } catch (const std::exception &e) {
        std::cerr << "Type inference error: " << e.what() << "\n";
    }

    return 0;
}
