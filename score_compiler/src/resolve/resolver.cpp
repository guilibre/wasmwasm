#include "resolver.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>

namespace {

constexpr size_t kMaxNodes = 100000;

using MemberSet = std::shared_ptr<std::unordered_set<size_t>>;

auto make_members(std::initializer_list<size_t> ids = {}) -> MemberSet {
    return std::make_shared<std::unordered_set<size_t>>(ids);
}

auto merge_members(const MemberSet &into, const MemberSet &from) -> void {
    if (into == from) return;
    into->insert(from->begin(), from->end());
}

struct PassthroughInfo {
    size_t id;
    std::optional<size_t> head_id;
    size_t tail_id;
    MemberSet members;
    std::optional<size_t> pending_target;
};

struct Piece {
    size_t entry_id;
    size_t exit_id;
    MemberSet members;
    std::shared_ptr<PassthroughInfo> open_passthrough;
};

auto link_after(std::vector<GraphNode> &nodes, const Piece &piece,
                size_t next_id) -> void {
    if (piece.open_passthrough) {
        PassthroughInfo &info = *piece.open_passthrough;
        if (info.head_id.has_value())
            nodes[info.tail_id].next.push_back(next_id);
        else
            info.head_id = next_id;
        info.tail_id = next_id;
    } else {
        nodes[piece.exit_id].next.push_back(next_id);
    }
}

struct PendingReverseClone {
    MemberSet members;
    size_t body_entry_id;
    size_t body_exit_id;
    MemberSet ring_members;
    size_t entry_anchor;
    size_t exit_anchor;
};

struct ProgressFrame {
    std::string name;
    std::shared_ptr<PassthroughInfo> passthrough;
};

struct ExpansionContext {
    std::unordered_map<std::string, const VarDecl *> symbols;
    std::unordered_map<std::string, const VarDecl *> shadow;
    std::vector<ProgressFrame> progress_stack;
    std::vector<GraphNode> nodes;
    std::vector<std::shared_ptr<PassthroughInfo>> passthroughs;
    std::vector<PendingReverseClone> pending_reverse_clones;
    std::unordered_map<size_t, size_t> transform_pop_of_push;
    std::vector<std::unique_ptr<Expr>> owned_exprs;

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
    if (expr.kind == Expr::Kind::Ident)
        throw ResolveException("identifier '" + expr.ident_name +
                                   "' is only allowed in a pipe expression",
                               expr.line, expr.column);
    if (expr.kind == Expr::Kind::Null)
        throw ResolveException("'null' is only allowed in a pipe expression",
                               expr.line, expr.column);
    if (expr.kind == Expr::Kind::Ternary)
        return fold_expr(*expr.ternary_cond) != 0
                   ? fold_expr(*expr.ternary_then)
                   : fold_expr(*expr.ternary_else);
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
    case BinOp::Pow:
        return std::pow(lhs, rhs);
    case BinOp::Eq:
        return lhs == rhs ? 1.0 : 0.0;
    case BinOp::NotEq:
        return lhs != rhs ? 1.0 : 0.0;
    case BinOp::Lt:
        return lhs < rhs ? 1.0 : 0.0;
    case BinOp::Gt:
        return lhs > rhs ? 1.0 : 0.0;
    case BinOp::LtEq:
        return lhs <= rhs ? 1.0 : 0.0;
    case BinOp::GtEq:
        return lhs >= rhs ? 1.0 : 0.0;
    }
    throw ResolveException("unknown operator", expr.line, expr.column);
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece;
auto expand_term(const Term &term, ExpansionContext &ctx) -> Piece;

auto expand_var_ref(const std::string &name, size_t line, size_t column,
                    ExpansionContext &ctx) -> Piece {
    for (auto it = ctx.progress_stack.rbegin(); it != ctx.progress_stack.rend();
         ++it) {
        if (it->name != name) continue;
        ProgressFrame &frame = *it;
        if (!frame.passthrough) {
            size_t id = ctx.alloc(NodeKind::Passthrough);
            frame.passthrough = std::make_shared<PassthroughInfo>(
                PassthroughInfo{.id = id,
                                .head_id = std::nullopt,
                                .tail_id = id,
                                .members = make_members({id}),
                                .pending_target = std::nullopt});
            ctx.passthroughs.push_back(frame.passthrough);
        }
        return Piece{
            .entry_id = frame.passthrough->id,
            .exit_id = frame.passthrough->id,
            .members = frame.passthrough->members,
            .open_passthrough = frame.passthrough,
        };
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
        return Piece{
            .entry_id = id,
            .exit_id = id,
            .members = make_members({id}),
            .open_passthrough = {},
        };
    }

    ctx.progress_stack.push_back(
        ProgressFrame{.name = name, .passthrough = nullptr});
    Piece body = expand_comp_expr(decl.comp, ctx);
    ProgressFrame frame = std::move(ctx.progress_stack.back());
    ctx.progress_stack.pop_back();

    if (!frame.passthrough) return body;

    frame.passthrough->pending_target = body.entry_id;
    merge_members(frame.passthrough->members, body.members);
    bool closes_own_cycle = body.open_passthrough == frame.passthrough;
    return Piece{
        .entry_id = frame.passthrough->id,
        .exit_id = frame.passthrough->id,
        .members = frame.passthrough->members,
        .open_passthrough =
            closes_own_cycle ? frame.passthrough : body.open_passthrough,
    };
}

auto make_expr(double value) -> std::unique_ptr<Expr> {
    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Number;
    expr->number = value;
    return expr;
}

struct AtomicResult {
    std::map<std::string, double> params;
    std::optional<std::string> instrument;
};

auto resolve_atomic_from_block(const Block &block) -> AtomicResult {
    AtomicResult result;
    for (const auto &param : block.params)
        result.params[param.name] = fold_expr(*param.value);
    result.instrument = block.instrument;
    return result;
}

auto resolve_atomic(const std::string &name, size_t line, size_t column,
                    ExpansionContext &ctx,
                    std::unordered_set<std::string> &visiting) -> AtomicResult;

auto resolve_atomic_decl(const VarDecl &decl, size_t line, size_t column,
                         ExpansionContext &ctx,
                         std::unordered_set<std::string> &visiting)
    -> AtomicResult {
    if (decl.kind == VarDecl::Kind::BlockDef)
        return resolve_atomic_from_block(decl.block);
    if (decl.kind == VarDecl::Kind::CompDef && decl.comp.terms.size() == 1) {
        const Term &term = decl.comp.terms[0];
        if (term.kind == Term::Kind::VarRef)
            return resolve_atomic(term.var_name, line, column, ctx, visiting);
        if (term.kind == Term::Kind::AtomicJoin &&
            term.lhs_expr->terms.size() == 1 &&
            term.lhs_expr->terms[0].kind == Term::Kind::VarRef) {
            AtomicResult a =
                resolve_atomic(term.lhs_expr->terms[0].var_name, term.line,
                               term.column, ctx, visiting);
            AtomicResult b;
            if (term.rhs_is_block) {
                b = resolve_atomic_from_block(term.rhs_block);
            } else {
                std::unordered_set<std::string> visiting_rhs;
                b = resolve_atomic(term.rhs_name, term.line, term.column, ctx,
                                   visiting_rhs);
            }
            for (auto &[key, value] : b.params) a.params[key] = value;
            if (b.instrument.has_value()) a.instrument = b.instrument;
            return a;
        }
    }
    throw ResolveException(
        "'@' operator requires atomic (block) variables on both sides", line,
        column);
}

auto resolve_atomic(const std::string &name, size_t line, size_t column,
                    ExpansionContext &ctx,
                    std::unordered_set<std::string> &visiting) -> AtomicResult {
    if (visiting.contains(name)) {
        auto shadow_it = ctx.shadow.find(name);
        if (shadow_it != ctx.shadow.end())
            return resolve_atomic_decl(*shadow_it->second, line, column, ctx,
                                       visiting);
        throw ResolveException("circular reference in '" + name + "'", line,
                               column);
    }
    auto it = ctx.symbols.find(name);
    if (it == ctx.symbols.end())
        throw ResolveException("undefined variable '" + name + "'", line,
                               column);
    visiting.insert(name);
    AtomicResult result =
        resolve_atomic_decl(*it->second, line, column, ctx, visiting);
    visiting.erase(name);
    return result;
}

auto expand_term(const Term &term, ExpansionContext &ctx) -> Piece {
    if (term.kind == Term::Kind::VarRef)
        return expand_var_ref(term.var_name, term.line, term.column, ctx);

    if (term.kind == Term::Kind::BlockLit) {
        size_t id = ctx.alloc(NodeKind::State);
        for (const auto &param : term.block_lit.params)
            ctx.nodes[id].params[param.name] = fold_expr(*param.value);
        ctx.nodes[id].instrument = term.block_lit.instrument;
        return Piece{
            .entry_id = id,
            .exit_id = id,
            .members = make_members({id}),
            .open_passthrough = {},
        };
    }

    if (term.kind == Term::Kind::AtomicJoin) {
        Piece lhs_piece = expand_comp_expr(*term.lhs_expr, ctx);

        AtomicResult b;
        if (term.rhs_is_block) {
            b = resolve_atomic_from_block(term.rhs_block);
        } else {
            std::unordered_set<std::string> visiting_rhs;
            b = resolve_atomic(term.rhs_name, term.line, term.column, ctx,
                               visiting_rhs);
        }

        size_t push_id = ctx.alloc(NodeKind::TransformPush);
        for (auto &[key, value] : b.params) {
            ctx.owned_exprs.push_back(make_expr(value));
            ctx.nodes[push_id].transforms.push_back(TransformEntry{
                .param_name = key, .expr = ctx.owned_exprs.back().get()});
        }
        ctx.nodes[push_id].push_instrument = b.instrument;
        ctx.nodes[push_id].next.push_back(lhs_piece.entry_id);

        if (lhs_piece.open_passthrough) {
            MemberSet members = make_members({push_id});
            merge_members(members, lhs_piece.members);
            return Piece{
                .entry_id = push_id,
                .exit_id = lhs_piece.exit_id,
                .members = members,
                .open_passthrough = lhs_piece.open_passthrough,
            };
        }

        size_t pop_id = ctx.alloc(NodeKind::TransformPop);
        link_after(ctx.nodes, lhs_piece, pop_id);
        ctx.transform_pop_of_push[push_id] = pop_id;

        MemberSet members = make_members({push_id, pop_id});
        merge_members(members, lhs_piece.members);
        return Piece{
            .entry_id = push_id,
            .exit_id = pop_id,
            .members = members,
            .open_passthrough = nullptr,
        };
    }

    if (term.kind == Term::Kind::Pipe) {
        if (term.pipe_op == Term::PipeOp::Reverse) {
            size_t entry_anchor = ctx.alloc(NodeKind::Passthrough);
            size_t exit_anchor = ctx.alloc(NodeKind::Passthrough);

            Piece body = expand_comp_expr(*term.lhs_expr, ctx);

            ctx.pending_reverse_clones.push_back(PendingReverseClone{
                .members = body.members,
                .body_entry_id = body.entry_id,
                .body_exit_id = body.exit_id,
                .ring_members = body.open_passthrough
                                    ? body.open_passthrough->members
                                    : nullptr,
                .entry_anchor = entry_anchor,
                .exit_anchor = exit_anchor,
            });

            return Piece{
                .entry_id = entry_anchor,
                .exit_id = exit_anchor,
                .members = make_members({entry_anchor, exit_anchor}),
                .open_passthrough = nullptr,
            };
        }

        size_t push_id = ctx.alloc(NodeKind::TransformPush);
        ctx.nodes[push_id].transforms.push_back(TransformEntry{
            .param_name = term.pipe_param_name, .expr = term.pipe_expr.get()});

        Piece lhs_piece = expand_comp_expr(*term.lhs_expr, ctx);
        ctx.nodes[push_id].next.push_back(lhs_piece.entry_id);

        if (lhs_piece.open_passthrough) {
            MemberSet members = make_members({push_id});
            merge_members(members, lhs_piece.members);
            return Piece{
                .entry_id = push_id,
                .exit_id = lhs_piece.exit_id,
                .members = members,
                .open_passthrough = lhs_piece.open_passthrough,
            };
        }

        size_t pop_id = ctx.alloc(NodeKind::TransformPop);
        link_after(ctx.nodes, lhs_piece, pop_id);
        ctx.transform_pop_of_push[push_id] = pop_id;

        MemberSet members = make_members({push_id, pop_id});
        merge_members(members, lhs_piece.members);
        return Piece{
            .entry_id = push_id,
            .exit_id = pop_id,
            .members = members,
            .open_passthrough = nullptr,
        };
    }

    std::vector<Piece> branches;
    branches.reserve(term.branches.size());
    for (const auto &branch : term.branches)
        branches.push_back(expand_comp_expr(*branch, ctx));

    if (branches.size() == 1) return branches[0];

    size_t fork_id = ctx.alloc(NodeKind::Fork);
    size_t join_id = ctx.alloc(NodeKind::Join);
    size_t infinite_branches = 0;
    for (const auto &branch : branches)
        if (branch.open_passthrough) ++infinite_branches;
    ctx.nodes[join_id].join_arity = branches.size() - infinite_branches;
    MemberSet members = make_members({fork_id, join_id});
    for (const auto &branch : branches) {
        ctx.nodes[fork_id].next.push_back(branch.entry_id);
        if (!branch.open_passthrough)
            ctx.nodes[branch.exit_id].next.push_back(join_id);
        merge_members(members, branch.members);
    }
    return Piece{
        .entry_id = fork_id,
        .exit_id = join_id,
        .members = members,
        .open_passthrough = {},
    };
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece {
    if (comp.terms.empty()) throw ResolveException("empty sequence", 0, 0);

    Piece first = expand_term(comp.terms.front(), ctx);
    if (comp.terms.size() == 1) return first;

    MemberSet members = make_members();
    merge_members(members, first.members);
    Piece prev = first;
    for (size_t i = 1; i < comp.terms.size(); ++i) {
        Piece next = expand_term(comp.terms[i], ctx);
        merge_members(members, next.members);
        if (prev.open_passthrough) continue;
        link_after(ctx.nodes, prev, next.entry_id);
        prev = Piece{
            .entry_id = prev.entry_id,
            .exit_id = next.exit_id,
            .members = nullptr,
            .open_passthrough = next.open_passthrough,
        };
    }
    return Piece{
        .entry_id = first.entry_id,
        .exit_id = prev.exit_id,
        .members = members,
        .open_passthrough = prev.open_passthrough,
    };
}

auto make_predefined_decls() -> std::vector<VarDecl> {
    constexpr std::array<std::pair<char, int>, 7> kMajorDegrees = {{
        {'a', 0},
        {'b', 2},
        {'c', 3},
        {'d', 5},
        {'e', 7},
        {'f', 8},
        {'g', 10},
    }};
    constexpr std::array<std::pair<const char *, int>, 3> kAccidentals = {{
        {"", 0},
        {"s", 1},
        {"b", -1},
    }};
    std::vector<VarDecl> decls;
    decls.reserve(21);
    for (const auto &[letter, degree] : kMajorDegrees) {
        for (const auto &[suffix, offset] : kAccidentals) {
            double freq = 440.0 * std::pow(2.0, (degree + offset) / 12.0);

            VarDecl decl;
            decl.name =
                std::string(1, static_cast<char>(std::toupper(letter))) +
                suffix;
            decl.kind = VarDecl::Kind::BlockDef;
            decl.block.params.push_back(
                Param{.name = "freq", .value = make_expr(freq)});
            decl.block.params.push_back(
                Param{.name = "dur", .value = make_expr(1)});
            decls.push_back(std::move(decl));
        }
    }
    return decls;
}

auto decl_is_atomic(
    const VarDecl &decl,
    const std::unordered_map<std::string, const VarDecl *> &symbols,
    std::unordered_set<std::string> &visiting) -> bool {
    if (decl.kind == VarDecl::Kind::BlockDef) return true;
    if (decl.kind != VarDecl::Kind::CompDef) return false;
    if (decl.comp.terms.size() != 1) return false;
    const Term &term = decl.comp.terms[0];
    if (term.kind == Term::Kind::AtomicJoin)
        return term.lhs_expr->terms.size() == 1 &&
               term.lhs_expr->terms[0].kind == Term::Kind::VarRef;
    if (term.kind != Term::Kind::VarRef) return false;
    if (visiting.contains(term.var_name)) return false;
    auto it = symbols.find(term.var_name);
    if (it == symbols.end()) return false;
    visiting.insert(term.var_name);
    bool result = decl_is_atomic(*it->second, symbols, visiting);
    visiting.erase(term.var_name);
    return result;
}

auto resolve_passthroughs(ExpansionContext &ctx) -> void {
    for (const auto &passthrough : ctx.passthroughs) {
        size_t from = passthrough->id;
        if (passthrough->head_id.has_value()) {
            ctx.nodes[from].next.push_back(*passthrough->head_id);
            from = passthrough->tail_id;
        }
        ctx.nodes[from].next.push_back(*passthrough->pending_target);
    }
}

auto prune_unreachable(ExpandedGraph &graph) -> void {
    std::vector<bool> reachable(graph.nodes.size(), false);
    std::vector<size_t> stack;
    for (const auto &machine : graph.entries)
        for (auto id : machine) stack.push_back(id);
    while (!stack.empty()) {
        size_t id = stack.back();
        stack.pop_back();
        if (reachable[id]) continue;
        reachable[id] = true;
        for (auto succ : graph.nodes[id].next)
            if (!reachable[succ]) stack.push_back(succ);
    }

    std::unordered_map<size_t, size_t> new_id;
    std::vector<GraphNode> kept;
    for (size_t id = 0; id < graph.nodes.size(); ++id) {
        if (!reachable[id]) continue;
        new_id[id] = kept.size();
        kept.push_back(std::move(graph.nodes[id]));
    }
    for (auto &node : kept) {
        node.id = new_id[node.id];
        for (auto &succ : node.next) succ = new_id[succ];
    }
    for (auto &machine : graph.entries)
        for (auto &id : machine) id = new_id[id];

    graph.nodes = std::move(kept);
}

auto resolve_pending_reverse_clones(ExpansionContext &ctx) -> void {
    std::unordered_map<size_t, size_t> resolved_bridges;

    for (const auto &pr : ctx.pending_reverse_clones) {
        auto all_members =
            std::make_shared<std::unordered_set<size_t>>(*pr.members);
        for (auto original : *pr.members) {
            auto bridge_it = resolved_bridges.find(original);
            if (bridge_it == resolved_bridges.end()) continue;
            size_t inner_exit = bridge_it->second;
            if (!pr.members->contains(inner_exit)) continue;
            size_t cur = original;
            while (cur != inner_exit && !ctx.nodes[cur].next.empty()) {
                size_t nxt = ctx.nodes[cur].next[0];
                if (nxt == inner_exit) break;
                if (!all_members->insert(nxt).second) break;
                if (pr.ring_members) pr.ring_members->insert(nxt);
                cur = nxt;
            }
        }

        std::unordered_map<size_t, size_t> clone_of;
        auto get_clone = [&](size_t original) -> size_t {
            auto it = clone_of.find(original);
            if (it != clone_of.end()) return it->second;
            size_t clone_id = ctx.nodes.size();
            GraphNode clone = ctx.nodes[original];
            clone.id = clone_id;
            clone.next.clear();
            ctx.nodes.push_back(std::move(clone));
            clone_of[original] = clone_id;
            return clone_id;
        };

        for (auto original : *all_members) get_clone(original);
        for (auto original : *all_members) {
            size_t clone_id = clone_of[original];
            for (auto succ : ctx.nodes[original].next) {
                if (!clone_of.contains(succ)) continue;
                bool flip = true;
                if (pr.ring_members) {
                    flip = pr.ring_members->contains(original) &&
                           pr.ring_members->contains(succ);
                }
                if (flip)
                    ctx.nodes[clone_of[succ]].next.push_back(clone_id);
                else
                    ctx.nodes[clone_id].next.push_back(clone_of[succ]);
            }
        }

        for (auto original : *all_members) {
            if (ctx.nodes[original].kind != NodeKind::TransformPush) continue;
            auto pop_it = ctx.transform_pop_of_push.find(original);
            if (pop_it == ctx.transform_pop_of_push.end()) continue;
            size_t original_pop = pop_it->second;
            if (!all_members->contains(original_pop)) continue;

            size_t push_clone = clone_of[original];
            size_t pop_clone = clone_of[original_pop];

            ctx.nodes[pop_clone].kind = NodeKind::TransformPush;
            ctx.nodes[pop_clone].transforms = ctx.nodes[original].transforms;
            ctx.nodes[pop_clone].push_instrument =
                ctx.nodes[original].push_instrument;

            ctx.nodes[push_clone].kind = NodeKind::TransformPop;
            ctx.nodes[push_clone].transforms.clear();
            ctx.nodes[push_clone].push_instrument = std::nullopt;

            ctx.transform_pop_of_push[pop_clone] = push_clone;
        }

        if (pr.ring_members) {
            ctx.nodes[pr.entry_anchor].next.push_back(
                clone_of[pr.body_entry_id]);
        } else {
            ctx.nodes[pr.entry_anchor].next.push_back(
                clone_of[pr.body_exit_id]);
            ctx.nodes[clone_of[pr.body_entry_id]].next.push_back(
                pr.exit_anchor);
        }

        resolved_bridges[pr.entry_anchor] = pr.exit_anchor;
    }
}

} // namespace

auto expand_program(const Program &program) -> ExpandedGraph {
    ExpansionContext ctx;

    std::vector<VarDecl> predefined_decls = make_predefined_decls();
    std::unordered_set<std::string> predefined_names;
    for (const auto &decl : predefined_decls) {
        predefined_names.insert(decl.name);
        ctx.symbols[decl.name] = &decl;
    }

    std::unordered_set<std::string> user_declared;
    for (const auto &decl : program.decls) {
        if (user_declared.contains(decl.name))
            throw ResolveException("duplicate variable '" + decl.name + "'",
                                   decl.line, decl.column);
        user_declared.insert(decl.name);

        if (predefined_names.contains(decl.name)) {
            std::unordered_set<std::string> visiting;
            if (!decl_is_atomic(decl, ctx.symbols, visiting))
                throw ResolveException(
                    "atomic variable '" + decl.name +
                        "' must be redefined with an atomic value",
                    decl.line, decl.column);
            ctx.shadow[decl.name] = ctx.symbols[decl.name];
        }

        ctx.symbols[decl.name] = &decl;
    }

    ExpandedGraph graph;
    for (const auto &decl : program.decls) {
        if (decl.kind != VarDecl::Kind::ScaleDef) continue;
        std::vector<double> values;
        values.reserve(decl.scale.size());
        for (const auto &elem : decl.scale) values.push_back(fold_expr(*elem));
        std::ranges::sort(values);
        values.erase(std::ranges::unique(values).begin(), values.end());
        graph.scales.emplace_back(decl.name, std::move(values));
    }

    for (const auto &play : program.plays) {
        for (const auto &machine : play.machines) {
            Piece piece = expand_comp_expr(machine, ctx);
            graph.entries.push_back({piece.entry_id});
        }
    }
    resolve_passthroughs(ctx);
    resolve_pending_reverse_clones(ctx);
    graph.nodes = std::move(ctx.nodes);
    graph.owned_exprs = std::move(ctx.owned_exprs);
    prune_unreachable(graph);
    return graph;
}
