#include "simplify.hpp"

#include <unordered_map>
#include <unordered_set>

namespace {

auto clone_expr(const Expr &expr) -> std::unique_ptr<Expr> {
    auto out = std::make_unique<Expr>();
    out->kind = expr.kind;
    out->number = expr.number;
    out->number_rational = expr.number_rational;
    out->string_value = expr.string_value;
    out->op = expr.op;
    out->ident_name = expr.ident_name;
    out->line = expr.line;
    out->column = expr.column;
    if (expr.lhs) out->lhs = clone_expr(*expr.lhs);
    if (expr.rhs) out->rhs = clone_expr(*expr.rhs);
    for (const auto &element : expr.elements)
        out->elements.push_back(clone_expr(*element));
    if (expr.ternary_cond) out->ternary_cond = clone_expr(*expr.ternary_cond);
    if (expr.ternary_then) out->ternary_then = clone_expr(*expr.ternary_then);
    if (expr.ternary_else) out->ternary_else = clone_expr(*expr.ternary_else);
    return out;
}

auto clone_param(const Param &param) -> Param {
    Param out;
    out.name = param.name;
    out.is_const = param.is_const;
    if (param.value) out.value = clone_expr(*param.value);
    return out;
}

auto clone_block(const Block &block) -> Block {
    Block out;
    out.line = block.line;
    out.column = block.column;
    for (const auto &param : block.params)
        out.params.push_back(clone_param(param));
    return out;
}

auto clone_comp_expr(const CompExpr &comp) -> CompExpr;

auto clone_term(const Term &term) -> Term {
    Term out;
    out.kind = term.kind;
    out.var_name = term.var_name;
    for (const auto &branch : term.branches)
        out.branches.push_back(
            std::make_unique<CompExpr>(clone_comp_expr(*branch)));
    if (term.lhs_expr)
        out.lhs_expr =
            std::make_unique<CompExpr>(clone_comp_expr(*term.lhs_expr));
    out.rhs_name = term.rhs_name;
    out.rhs_is_block = term.rhs_is_block;
    out.rhs_block = clone_block(term.rhs_block);
    out.block_lit = clone_block(term.block_lit);
    out.pipe_op = term.pipe_op;
    if (term.pipe_expr) out.pipe_expr = clone_expr(*term.pipe_expr);
    out.legato_after = term.legato_after;
    out.line = term.line;
    out.column = term.column;
    return out;
}

auto clone_comp_expr(const CompExpr &comp) -> CompExpr {
    CompExpr out;
    out.terms.reserve(comp.terms.size());
    for (const auto &term : comp.terms) out.terms.push_back(clone_term(term));
    return out;
}

auto clone_var_decl(const VarDecl &decl) -> VarDecl {
    VarDecl out;
    out.name = decl.name;
    out.kind = decl.kind;
    out.block = clone_block(decl.block);
    out.comp = clone_comp_expr(decl.comp);
    for (const auto &elem : decl.scale) out.scale.push_back(clone_expr(*elem));
    out.line = decl.line;
    out.column = decl.column;
    return out;
}

auto clone_play_stmt(const PlayStmt &play) -> PlayStmt {
    PlayStmt out;
    out.line = play.line;
    out.column = play.column;
    for (const auto &machine : play.machines)
        out.machines.push_back(clone_comp_expr(machine));
    return out;
}

auto simplify_comp_expr(CompExpr &comp,
                        std::unordered_map<std::string, VarDecl *> &decls,
                        std::unordered_set<std::string> &visiting) -> void;

auto simplify_decl(VarDecl &decl,
                   std::unordered_map<std::string, VarDecl *> &decls,
                   std::unordered_set<std::string> &visiting) -> void {
    if (decl.kind != VarDecl::Kind::CompDef) return;
    if (visiting.contains(decl.name)) return;
    visiting.insert(decl.name);
    simplify_comp_expr(decl.comp, decls, visiting);
    visiting.erase(decl.name);
}

auto simplify_comp_expr(CompExpr &comp,
                        std::unordered_map<std::string, VarDecl *> &decls,
                        std::unordered_set<std::string> &visiting) -> void {
    for (auto &term : comp.terms) {
        for (auto &branch : term.branches)
            simplify_comp_expr(*branch, decls, visiting);
        if (term.lhs_expr) simplify_comp_expr(*term.lhs_expr, decls, visiting);
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < comp.terms.size(); ++i) {
            auto &term = comp.terms[i];

            if (term.kind != Term::Kind::VarRef) continue;

            const auto it = decls.find(term.var_name);
            if (it == decls.end()) continue;
            auto *target = it->second;
            if (target->kind != VarDecl::Kind::CompDef) continue;
            if (visiting.contains(target->name)) continue;

            simplify_decl(*target, decls, visiting);

            std::vector<Term> replacement;
            replacement.reserve(target->comp.terms.size());
            for (const auto &t : target->comp.terms)
                replacement.push_back(clone_term(t));

            comp.terms.erase(comp.terms.begin() + static_cast<ptrdiff_t>(i));
            comp.terms.insert(comp.terms.begin() + static_cast<ptrdiff_t>(i),
                              std::make_move_iterator(replacement.begin()),
                              std::make_move_iterator(replacement.end()));

            changed = true;
            break;
        }
    }
}

} // namespace

auto simplify_program(const Program &program) -> Program {
    Program out;
    out.decls.reserve(program.decls.size());
    for (const auto &decl : program.decls)
        out.decls.push_back(clone_var_decl(decl));
    out.plays.reserve(program.plays.size());
    for (const auto &play : program.plays)
        out.plays.push_back(clone_play_stmt(play));

    std::unordered_map<std::string, VarDecl *> decls;
    for (auto &decl : out.decls) decls[decl.name] = &decl;

    for (auto &decl : out.decls) {
        std::unordered_set<std::string> visiting;
        simplify_decl(decl, decls, visiting);
    }

    for (auto &play : out.plays)
        for (auto &machine : play.machines) {
            std::unordered_set<std::string> visiting;
            simplify_comp_expr(machine, decls, visiting);
        }

    return out;
}
