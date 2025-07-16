#include "free_var_analyzer.hpp"
#include <unordered_set>

namespace FreeVarAnalyzer {

auto analyze(const ExprPtr &expr,
             const std::unordered_set<std::string_view> &bound)
    -> std::unordered_set<std::string_view> {
    using Set = std::unordered_set<std::string_view>;

    struct Visitor {
        Set bound;
        Set result;

        void visit(const ExprPtr &e) { // NOLINT
            std::visit([&](const auto &node) { visit_node(node); }, e->node);
        }

        void visit_node(const Expr::Literal & /*unused*/) {}

        void visit_node(const Expr::Variable &v) {
            auto name = std::string_view(v.name.lexeme);
            if (!bound.contains(name)) result.insert(name);
        }

        void visit_node(const Expr::Assignment &a) { visit(a.value); }

        void visit_node(const Expr::Call &c) {
            visit(c.callee);
            if (c.argument) visit(c.argument);
        }

        void visit_node(const Expr::Block &b) {
            for (const auto &e : b.expressions)
                visit(e);
        }

        void visit_node(const Expr::Lambda &l) {
            Set inner_bound = bound;
            inner_bound.insert(std::string_view(l.parameter.lexeme));

            auto inner_free = analyze(l.body, inner_bound);
            result.insert(inner_free.begin(), inner_free.end());
        }
    };

    Visitor v{.bound = bound};
    v.visit(expr);
    return v.result;
}

} // namespace FreeVarAnalyzer