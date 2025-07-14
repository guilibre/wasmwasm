#include "ast.hpp"

#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>

SubstitutionVisitor::SubstitutionVisitor(
    std::unordered_map<std::string, ExprPtr> replacements)
    : replacements(std::move(replacements)) {}

auto SubstitutionVisitor::visit(ExprPtr expr) -> ExprPtr {
    return std::visit(*this, expr->node);
}

auto SubstitutionVisitor::operator()(Expr::Assignment &assign) -> ExprPtr {
    return Expr::make<Expr::Assignment>(visit(std::move(assign.value)),
                                        std::move(assign.name));
}

auto SubstitutionVisitor::operator()(Expr::Binary &bin) -> ExprPtr {
    return Expr::make<Expr::Binary>(bin.op, visit(std::move(bin.lhs)),
                                    visit(std::move(bin.rhs)));
}

auto SubstitutionVisitor::operator()(Expr::Block &block) -> ExprPtr {
    std::vector<ExprPtr> result;
    result.reserve(block.size());
    for (auto &expr : block)
        result.emplace_back(visit(std::move(expr)));

    return Expr::make<Expr::Block>(std::move(result));
}

auto SubstitutionVisitor::operator()(Expr::Call &call) -> ExprPtr {
    return Expr::make<Expr::Call>(visit(std::move(call.callee)),
                                  visit(std::move(call.argument)));
}

auto SubstitutionVisitor::operator()(Expr::Lambda &lambda) -> ExprPtr {
    return Expr::make<Expr::Lambda>(std::move(lambda.parameters),
                                    visit(std::move(lambda.body)));
}

auto SubstitutionVisitor::operator()(Expr::Literal &lit) -> ExprPtr {
    return Expr::make<Expr::Literal>(std::move(lit));
}

auto SubstitutionVisitor::operator()(Expr::Variable &var) -> ExprPtr {
    if (replacements.contains(var.name.lexeme)) {
        return std::move(replacements.at(var.name.lexeme));
    }
    return Expr::make<Expr::Variable>(var);
}

void LambdaInliner::collect_arguments(ExprPtr expr,
                                      std::vector<ExprPtr> &args) {
    auto *call = std::get_if<Expr::Call>(&expr->node);
    if (call == nullptr) {
        args.push_back(std::move(expr));
    } else {
        collect_arguments(std::move(call->callee), args);
        args.push_back(std::move(call->argument));
    }
}

auto LambdaInliner::apply_arguments(ExprPtr lambda, std::vector<ExprPtr> args)
    -> ExprPtr {
    for (auto &arg : args) {
        auto *lam = std::get_if<Expr::Lambda>(&lambda->node);
        if (lam == nullptr || lam->parameters.empty()) break;

        std::unordered_map<std::string, ExprPtr> subs;
        subs[lam->parameters[0].name.lexeme] = std::move(arg);

        if (lam->parameters.size() <= 1) {
            SubstitutionVisitor substitution_visitor(std::move(subs));
            lambda = substitution_visitor.visit(std::move(lam->body));
        } else {
            std::vector<Expr::Variable> remaining_params(
                lam->parameters.begin() + 1, lam->parameters.end());
            SubstitutionVisitor substitution_visitor(std::move(subs));
            lambda = Expr::make<Expr::Lambda>(
                std::move(remaining_params),
                substitution_visitor.visit(std::move(lam->body)));
        }
    }
    return lambda;
}

auto LambdaInliner::visit(ExprPtr expr) -> ExprPtr {
    if (!expr) return nullptr;
    return std::visit(*this, expr->node);
}

auto LambdaInliner::operator()(Expr::Assignment &assign) -> ExprPtr {
    return Expr::make<Expr::Assignment>(visit(std::move(assign.value)),
                                        std::move(assign.name));
}

auto LambdaInliner::operator()(Expr::Binary &bin) -> ExprPtr {
    return Expr::make<Expr::Binary>(bin.op, visit(std::move(bin.lhs)),
                                    visit(std::move(bin.rhs)));
}

auto LambdaInliner::operator()(Expr::Block &block) -> ExprPtr {
    std::vector<ExprPtr> result;
    result.reserve(block.size());
    for (auto &expr : block)
        result.emplace_back(visit(std::move(expr)));

    return Expr::make<Expr::Block>(std::move(result));
}

auto LambdaInliner::operator()(Expr::Call &call) -> ExprPtr {
    std::vector<ExprPtr> args;
    collect_arguments(Expr::make<Expr::Call>(std::move(call.callee),
                                             std::move(call.argument)),
                      args);

    ExprPtr lambda = visit(std::move(args.back()));
    args.pop_back();

    for (auto &arg : args)
        arg = visit(std::move(arg));

    if (auto result = apply_arguments(std::move(lambda), std::move(args)))
        return result;

    ExprPtr current = std::move(lambda);
    for (auto &arg : std::ranges::reverse_view(args))
        current = Expr::make<Expr::Call>(std::move(current), std::move(arg));

    return current;
}

auto LambdaInliner::operator()(Expr::Lambda &lambda) -> ExprPtr {
    return Expr::make<Expr::Lambda>(std::move(lambda.parameters),
                                    visit(std::move(lambda.body)));
}

auto LambdaInliner::operator()(Expr::Literal &lit) -> ExprPtr {
    return Expr::make<Expr::Literal>(std::move(lit));
}

auto LambdaInliner::operator()(Expr::Variable &var) -> ExprPtr {
    return Expr::make<Expr::Variable>(std::move(var));
}

auto ASTPrinter::print(const Expr &expr) -> std::string {
    return std::visit(*this, expr.node);
}

auto ASTPrinter::operator()(const Expr::Literal &lit) -> std::string {
    return lit.value.lexeme;
}

auto ASTPrinter::operator()(const Expr::Variable &var) -> std::string {
    return var.name.lexeme;
}

auto ASTPrinter::operator()(const Expr::Assignment &asg) -> std::string {
    return print(*asg.value) + " -> " + asg.name.lexeme;
}

auto ASTPrinter::operator()(const Expr::Binary &bin) -> std::string {
    return "(" + print(*bin.lhs) + " " + token_to_string(bin.op.kind) + " " +
           print(*bin.rhs) + ")";
}

auto ASTPrinter::operator()(const Expr::Call &call) -> std::string {
    return print(*call.callee) + "(" +
           (call.argument ? print(*call.argument) : "") + ")";
}

auto ASTPrinter::operator()(const Expr::Block &block) -> std::string {
    std::string result = "{\n";
    for (const auto &expr : block) {
        result += "  " + print(*expr) + ";\n";
    }
    return result + "}";
}

auto ASTPrinter::operator()(const Expr::Lambda &lambda) -> std::string {
    std::string params;
    for (const auto &param : lambda.parameters) {
        if (!params.empty()) params += ", ";
        params += param.name.lexeme;
    }
    return "λ(" + params + "). " + print(*lambda.body);
}

auto ASTPrinter::token_to_string(TokenKind kind) -> std::string {
    switch (kind) {
    case TokenKind::Plus:
        return "+";
    case TokenKind::Minus:
        return "-";
    case TokenKind::Star:
        return "*";
    case TokenKind::Slash:
        return "/";
    case TokenKind::Arrow:
        return "->";
    default:
        return "<?>";
    }
}