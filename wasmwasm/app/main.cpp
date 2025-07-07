#include "code_gen.hpp"

#include "parser.hpp"
#include "tokenizer.hpp"

#include <exception>
#include <iostream>
#include <variant>

extern "C" auto run_compiler(float sample_freq) -> int {
    return code_gen::test(sample_freq);
}

namespace {

template <class T> struct always_false : std::false_type {};

void print_expr(const Expr *expr, int indent = 0) {
    std::string pad(indent, ' ');
    std::visit(
        [&](auto &&node) -> void {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, typename Expr::Binary>) {
                std::cout << pad << "Binary(" << node.op.lexeme << ")\n";
                print_expr(node.left.get(), indent + 2);
                print_expr(node.right.get(), indent + 2);
            } else if constexpr (std::is_same_v<T, typename Expr::Literal>) {
                std::cout << pad << "Literal(" << node.value.lexeme << ")\n";
            } else if constexpr (std::is_same_v<T, typename Expr::Variable>) {
                std::cout << pad << "Variable(" << node.name.lexeme << ")\n";
            } else if constexpr (std::is_same_v<T, typename Expr::If>) {
                std::cout << pad << "If\n";
                print_expr(node.condition.get(), indent + 2);
                print_expr(node.then_branch.get(), indent + 2);
                print_expr(node.else_branch.get(), indent + 2);
            } else if constexpr (std::is_same_v<T, typename Expr::Assignment>) {
                std::cout << pad << "Assignment to " << node.name.lexeme
                          << "\n";
                print_expr(node.value.get(), indent + 2);
            } else {
                static_assert(always_false<T>::value,
                              "Non-exhaustive visitor!");
            }
        },
        *(expr->node));
}

} // namespace

auto main(int /*argc*/, char ** /*argv*/) -> int {
    const auto *source = "if x == 1 then 42 else x -> z";

    Tokenizer tokenizer(source);
    Parser parser(tokenizer);

    try {
        auto ast = parser.parse_expr();
        print_expr(ast.get());
    } catch (const std::exception &ex) {
        std::cerr << "Parse error: " << ex.what() << '\n';
        return 1;
    }

    return 0;
}