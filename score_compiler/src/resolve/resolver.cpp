#include "resolver.hpp"

#include <optional>
#include <unordered_map>

namespace {

constexpr size_t kMaxNodes = 100000;

struct Piece {
    size_t entry_id;
    size_t exit_id;
};

struct ExpansionContext {
    std::unordered_map<std::string, const VarDecl *> symbols;
    std::unordered_map<std::string, std::optional<size_t>> in_progress;
    std::vector<GraphNode> nodes;

    auto alloc(NodeKind kind) -> size_t {
        if (nodes.size() >= kMaxNodes)
            throw ResolveException("graph exceeded maximum node count", 0, 0);
        const auto id = nodes.size();
        GraphNode node;
        node.id = id;
        node.kind = kind;
        nodes.push_back(std::move(node));
        return id;
    }
};

auto fold_expr(const Expr &expr) -> double {
    if (expr.kind == Expr::Kind::Number) return expr.number;
    double lhs = fold_expr(*expr.lhs);
    double rhs = fold_expr(*expr.rhs);
    switch (expr.op) {
    case BinOp::Add:
        return lhs + rhs;
    case BinOp::Sub:
        return lhs - rhs;
    case BinOp::Mul:
        return lhs * rhs;
    case BinOp::Div:
        if (rhs == 0)
            throw ResolveException("division by zero", expr.line, expr.column);
        return lhs / rhs;
    }
    throw ResolveException("unknown operator", expr.line, expr.column);
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece;

auto expand_var_ref(const std::string &name, size_t line, size_t column,
                    ExpansionContext &ctx) -> Piece {
    auto in_progress_it = ctx.in_progress.find(name);
    if (in_progress_it != ctx.in_progress.end()) {
        if (!in_progress_it->second.has_value())
            in_progress_it->second = ctx.alloc(NodeKind::Passthrough);
        size_t id = *in_progress_it->second;
        return Piece{.entry_id = id, .exit_id = id};
    }

    auto symbol_it = ctx.symbols.find(name);
    if (symbol_it == ctx.symbols.end())
        throw ResolveException("undefined variable '" + name + "'", line,
                               column);
    const VarDecl &decl = *symbol_it->second;

    if (decl.kind == VarDecl::Kind::BlockDef) {
        size_t id = ctx.alloc(NodeKind::State);
        for (const auto &param : decl.block.params)
            ctx.nodes[id].params[param.name] = fold_expr(*param.value);
        ctx.nodes[id].instrument = decl.block.instrument;
        return Piece{.entry_id = id, .exit_id = id};
    }

    ctx.in_progress[name] = std::nullopt;
    Piece body = expand_comp_expr(decl.comp, ctx);
    std::optional<size_t> placeholder_id = ctx.in_progress.at(name);
    ctx.in_progress.erase(name);

    if (!placeholder_id.has_value()) return body;

    ctx.nodes[*placeholder_id].next.push_back(static_cast<int>(body.entry_id));
    return Piece{.entry_id = *placeholder_id, .exit_id = body.exit_id};
}

auto expand_term(const Term &term, ExpansionContext &ctx) -> Piece {
    if (term.kind == Term::Kind::VarRef)
        return expand_var_ref(term.var_name, term.line, term.column, ctx);

    std::vector<Piece> branches;
    branches.reserve(term.branches.size());
    for (const auto &branch : term.branches)
        branches.push_back(expand_comp_expr(*branch, ctx));

    if (branches.size() == 1) return branches[0];

    size_t fork_id = ctx.alloc(NodeKind::Fork);
    size_t join_id = ctx.alloc(NodeKind::Join);
    ctx.nodes[join_id].join_arity = branches.size();
    for (const auto &branch : branches) {
        ctx.nodes[fork_id].next.push_back(static_cast<int>(branch.entry_id));
        ctx.nodes[branch.exit_id].next.push_back(static_cast<int>(join_id));
    }
    return Piece{.entry_id = fork_id, .exit_id = join_id};
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece {
    if (comp.terms.empty()) throw ResolveException("empty sequence", 0, 0);

    Piece first = expand_term(comp.terms.front(), ctx);
    Piece prev = first;
    for (size_t i = 1; i < comp.terms.size(); ++i) {
        Piece next = expand_term(comp.terms[i], ctx);
        ctx.nodes[prev.exit_id].next.push_back(static_cast<int>(next.entry_id));
        prev = Piece{.entry_id = prev.entry_id, .exit_id = next.exit_id};
    }
    return Piece{.entry_id = first.entry_id, .exit_id = prev.exit_id};
}

} // namespace

auto expand_program(const Program &program) -> ExpandedGraph {
    ExpansionContext ctx;
    for (const auto &decl : program.decls) {
        if (ctx.symbols.contains(decl.name))
            throw ResolveException("duplicate variable '" + decl.name + "'",
                                   decl.line, decl.column);
        ctx.symbols[decl.name] = &decl;
    }

    ExpandedGraph graph;
    for (const auto &play : program.plays) {
        for (const auto &machine : play.machines) {
            Piece piece = expand_comp_expr(machine, ctx);
            graph.entries.push_back({piece.entry_id});
        }
    }
    graph.nodes = std::move(ctx.nodes);
    return graph;
}
