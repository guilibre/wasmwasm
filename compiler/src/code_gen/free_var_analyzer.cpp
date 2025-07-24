#include "free_var_analyzer.hpp"
#include "code_gen_context.hpp"

#include <functional>
#include <memory>
#include <type_traits>

namespace FreeVarAnalyzer {

auto analyze(const ExprPtr &expr, const std::unordered_set<std::string> &bound,
             const std::shared_ptr<CodeGenContext> &ctx)
    -> std::unordered_set<std::string> {
    std::unordered_set<std::string> result;

    std::function<void(const ExprPtr &)> visit = [&](const ExprPtr &e) {
        std::visit(
            [&](const auto &node) {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Expr::Assignment>) {
                    visit(node.value);
                } else if constexpr (std::is_same_v<T, Expr::Block>) {
                    for (const auto &subexpr : node.expressions)
                        visit(subexpr);
                } else if constexpr (std::is_same_v<T, Expr::Call>) {
                    visit(node.callee);
                    visit(node.argument);
                } else if constexpr (std::is_same_v<T, Expr::Lambda>) {
                    auto inner_bound = bound;
                    inner_bound.emplace(node.parameter.lexeme);
                    auto inner_free = analyze(node.body, inner_bound, ctx);
                    result.insert(inner_free.begin(), inner_free.end());
                } else if constexpr (std::is_same_v<T, Expr::Variable>) {
                    if (!bound.contains(node.name.lexeme) &&
                        node.name.lexeme != "TIME" &&
                        !ctx->has_function(node.name.lexeme) &&
                        !ctx->has_constant(node.name.lexeme))
                        result.emplace(node.name.lexeme);
                }
            },
            e->node);
    };

    visit(expr);
    return result;
}

} // namespace FreeVarAnalyzer