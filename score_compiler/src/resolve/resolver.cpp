#include "resolver.hpp"

#include "optimize.hpp"

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
        auto &info = *piece.open_passthrough;
        if (info.head_id.has_value())
            nodes[info.tail_id].next.push_back(next_id);
        else
            info.head_id = next_id;
        info.tail_id = next_id;
    } else {
        nodes[piece.exit_id].next.push_back(next_id);
    }
}

struct PendingLegatoBoundary {
    size_t passthrough_id;
    bool has_after;
    size_t chain_id;
    size_t line;
    size_t column;
};

struct PendingReverseClone {
    MemberSet members;
    size_t body_entry_id;
    size_t body_exit_id;
    MemberSet ring_members;
    size_t entry_anchor;
    size_t exit_anchor;
};

struct PendingRepeatClone {
    MemberSet members;
    size_t body_entry_id;
    size_t body_exit_id;
    size_t entry_anchor;
    size_t exit_anchor;
    size_t count;
};

struct ProgressFrame {
    std::string name;
    std::shared_ptr<PassthroughInfo> passthrough;
};

struct ExpansionContext {
    std::unordered_map<std::string, const VarDecl *> symbols;
    std::unordered_map<std::string, const VarDecl *> shadow;
    std::unordered_map<std::string, size_t> scale_index;
    std::vector<ProgressFrame> progress_stack;
    std::vector<GraphNode> nodes;
    std::vector<std::shared_ptr<PassthroughInfo>> passthroughs;
    std::vector<PendingReverseClone> pending_reverse_clones;
    std::vector<PendingRepeatClone> pending_repeat_clones;
    std::vector<PendingLegatoBoundary> pending_legato_boundaries;
    std::unordered_map<size_t, size_t> transform_pop_of_push;
    std::vector<std::unique_ptr<Expr>> owned_exprs;
    size_t next_legato_id = 0;

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
    const auto lhs = fold_expr(*expr.lhs);
    const auto rhs = fold_expr(*expr.rhs);
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

auto fold_block_params(const std::vector<Param> &params, ExpansionContext &ctx)
    -> std::map<std::string, double> {
    std::map<std::string, double> result;
    for (const auto &param : params) {
        if (param.name == "scale") {
            if (param.value->kind != Expr::Kind::Ident)
                throw ResolveException("'scale' field requires a scale name",
                                       param.value->line, param.value->column);
            const auto it = ctx.scale_index.find(param.value->ident_name);
            if (it == ctx.scale_index.end())
                throw ResolveException("undefined scale '" +
                                           param.value->ident_name + "'",
                                       param.value->line, param.value->column);
            result[param.name] = static_cast<double>(it->second);
            continue;
        }
        result[param.name] = fold_expr(*param.value);
    }
    return result;
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece;
auto expand_term(const Term &term, ExpansionContext &ctx) -> Piece;

auto expand_var_ref(const std::string &name, size_t line, size_t column,
                    ExpansionContext &ctx) -> Piece {
    for (auto it = ctx.progress_stack.rbegin(); it != ctx.progress_stack.rend();
         ++it) {
        if (it->name != name) continue;
        auto &frame = *it;
        if (!frame.passthrough) {
            const auto id = ctx.alloc(NodeKind::Passthrough);
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

    const auto symbol_it = ctx.symbols.find(name);
    if (symbol_it == ctx.symbols.end())
        throw ResolveException("undefined variable '" + name + "'", line,
                               column);
    const auto &decl = *symbol_it->second;

    if (decl.kind == VarDecl::Kind::BlockDef) {
        const auto id = ctx.alloc(NodeKind::State);
        for (auto &[key, value] : fold_block_params(decl.block.params, ctx))
            ctx.nodes[id].params[key] = value;
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
    auto body = expand_comp_expr(decl.comp, ctx);
    const auto frame = std::move(ctx.progress_stack.back());
    ctx.progress_stack.pop_back();

    if (!frame.passthrough) return body;

    frame.passthrough->pending_target = body.entry_id;
    merge_members(frame.passthrough->members, body.members);
    const auto closes_own_cycle = body.open_passthrough == frame.passthrough;
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

auto resolve_atomic_from_block(const Block &block, ExpansionContext &ctx)
    -> AtomicResult {
    AtomicResult result;
    result.params = fold_block_params(block.params, ctx);
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
        return resolve_atomic_from_block(decl.block, ctx);
    if (decl.kind == VarDecl::Kind::CompDef && decl.comp.terms.size() == 1) {
        const auto &term = decl.comp.terms[0];
        if (term.kind == Term::Kind::VarRef)
            return resolve_atomic(term.var_name, line, column, ctx, visiting);
        if (term.kind == Term::Kind::AtomicJoin &&
            term.lhs_expr->terms.size() == 1 &&
            term.lhs_expr->terms[0].kind == Term::Kind::VarRef) {
            auto a = resolve_atomic(term.lhs_expr->terms[0].var_name, term.line,
                                    term.column, ctx, visiting);
            AtomicResult b;
            if (term.rhs_is_block) {
                b = resolve_atomic_from_block(term.rhs_block, ctx);
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
        const auto shadow_it = ctx.shadow.find(name);
        if (shadow_it != ctx.shadow.end())
            return resolve_atomic_decl(*shadow_it->second, line, column, ctx,
                                       visiting);
        throw ResolveException("circular reference in '" + name + "'", line,
                               column);
    }
    const auto it = ctx.symbols.find(name);
    if (it == ctx.symbols.end())
        throw ResolveException("undefined variable '" + name + "'", line,
                               column);
    visiting.insert(name);
    const auto result =
        resolve_atomic_decl(*it->second, line, column, ctx, visiting);
    visiting.erase(name);
    return result;
}

auto expand_term(const Term &term, ExpansionContext &ctx) -> Piece {
    if (term.kind == Term::Kind::VarRef)
        return expand_var_ref(term.var_name, term.line, term.column, ctx);

    if (term.kind == Term::Kind::BlockLit) {
        const auto id = ctx.alloc(NodeKind::State);
        for (auto &[key, value] : fold_block_params(term.block_lit.params, ctx))
            ctx.nodes[id].params[key] = value;
        ctx.nodes[id].instrument = term.block_lit.instrument;
        return Piece{
            .entry_id = id,
            .exit_id = id,
            .members = make_members({id}),
            .open_passthrough = {},
        };
    }

    if (term.kind == Term::Kind::AtomicJoin) {
        const auto lhs_piece = expand_comp_expr(*term.lhs_expr, ctx);

        AtomicResult b;
        if (term.rhs_is_block) {
            b = resolve_atomic_from_block(term.rhs_block, ctx);
        } else {
            std::unordered_set<std::string> visiting_rhs;
            b = resolve_atomic(term.rhs_name, term.line, term.column, ctx,
                               visiting_rhs);
        }

        const auto push_id = ctx.alloc(NodeKind::TransformPush);
        for (auto &[key, value] : b.params) {
            ctx.owned_exprs.push_back(make_expr(value));
            ctx.nodes[push_id].transforms.push_back(TransformEntry{
                .param_name = key, .expr = ctx.owned_exprs.back().get()});
        }
        ctx.nodes[push_id].push_instrument = b.instrument;
        ctx.nodes[push_id].next.push_back(lhs_piece.entry_id);

        if (lhs_piece.open_passthrough) {
            const auto members = make_members({push_id});
            merge_members(members, lhs_piece.members);
            return Piece{
                .entry_id = push_id,
                .exit_id = lhs_piece.exit_id,
                .members = members,
                .open_passthrough = lhs_piece.open_passthrough,
            };
        }

        const auto pop_id = ctx.alloc(NodeKind::TransformPop);
        link_after(ctx.nodes, lhs_piece, pop_id);
        ctx.transform_pop_of_push[push_id] = pop_id;

        const auto members = make_members({push_id, pop_id});
        merge_members(members, lhs_piece.members);
        return Piece{
            .entry_id = push_id,
            .exit_id = pop_id,
            .members = members,
            .open_passthrough = nullptr,
        };
    }

    if (term.kind == Term::Kind::Pipe) {
        if (term.pipe_op == Term::PipeOp::Repeat) {
            const auto count_value = fold_expr(*term.pipe_expr);
            if (count_value < 1)
                throw ResolveException("'repeat' count must be at least 1",
                                       term.line, term.column);
            const auto count = static_cast<size_t>(std::floor(count_value));

            if (count == 1) return expand_comp_expr(*term.lhs_expr, ctx);

            const auto body = expand_comp_expr(*term.lhs_expr, ctx);
            if (body.open_passthrough)
                throw ResolveException(
                    "'repeat' cannot wrap a composition that already "
                    "repeats infinitely",
                    term.line, term.column);

            const auto entry_anchor = ctx.alloc(NodeKind::Passthrough);
            const auto exit_anchor = ctx.alloc(NodeKind::Passthrough);

            ctx.pending_repeat_clones.push_back(PendingRepeatClone{
                .members = body.members,
                .body_entry_id = body.entry_id,
                .body_exit_id = body.exit_id,
                .entry_anchor = entry_anchor,
                .exit_anchor = exit_anchor,
                .count = count,
            });

            return Piece{
                .entry_id = entry_anchor,
                .exit_id = exit_anchor,
                .members = make_members({entry_anchor, exit_anchor}),
                .open_passthrough = nullptr,
            };
        }

        if (term.pipe_op == Term::PipeOp::Reverse) {
            const auto entry_anchor = ctx.alloc(NodeKind::Passthrough);
            const auto exit_anchor = ctx.alloc(NodeKind::Passthrough);

            const auto body = expand_comp_expr(*term.lhs_expr, ctx);

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

        const auto push_id = ctx.alloc(NodeKind::TransformPush);
        ctx.nodes[push_id].transforms.push_back(TransformEntry{
            .param_name = term.pipe_param_name, .expr = term.pipe_expr.get()});

        const auto lhs_piece = expand_comp_expr(*term.lhs_expr, ctx);
        ctx.nodes[push_id].next.push_back(lhs_piece.entry_id);

        if (lhs_piece.open_passthrough) {
            const auto members = make_members({push_id});
            merge_members(members, lhs_piece.members);
            return Piece{
                .entry_id = push_id,
                .exit_id = lhs_piece.exit_id,
                .members = members,
                .open_passthrough = lhs_piece.open_passthrough,
            };
        }

        const auto pop_id = ctx.alloc(NodeKind::TransformPop);
        link_after(ctx.nodes, lhs_piece, pop_id);
        ctx.transform_pop_of_push[push_id] = pop_id;

        const auto members = make_members({push_id, pop_id});
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

    const auto fork_id = ctx.alloc(NodeKind::Fork);
    const auto join_id = ctx.alloc(NodeKind::Join);
    size_t infinite_branches = 0;
    for (const auto &branch : branches)
        if (branch.open_passthrough) ++infinite_branches;
    ctx.nodes[join_id].join_arity = branches.size() - infinite_branches;
    const auto members = make_members({fork_id, join_id});
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

auto find_boundary_state(ExpansionContext &ctx, size_t entry_id, size_t exit_id)
    -> std::optional<size_t> {
    std::optional<size_t> last_state;
    auto id = entry_id;
    while (true) {
        if (ctx.nodes[id].kind == NodeKind::State) last_state = id;
        if (id == exit_id) break;
        if (ctx.nodes[id].next.size() != 1) return std::nullopt;
        id = ctx.nodes[id].next[0];
    }
    return last_state;
}

auto find_first_state(ExpansionContext &ctx, size_t start_id)
    -> std::optional<size_t> {
    auto id = start_id;
    while (ctx.nodes[id].kind != NodeKind::State) {
        if (ctx.nodes[id].next.size() != 1) return std::nullopt;
        id = ctx.nodes[id].next[0];
    }
    return id;
}

auto apply_legato(ExpansionContext &ctx, const Piece &piece, const Term &term,
                  bool in_chain, bool has_after,
                  std::optional<size_t> &chain_id) -> void {
    if (!in_chain) {
        chain_id.reset();
        return;
    }
    if (!chain_id) chain_id = ctx.next_legato_id++;

    const auto state_id =
        find_boundary_state(ctx, piece.entry_id, piece.exit_id);
    if (state_id) {
        ctx.nodes[*state_id].params["legato"] = has_after ? 1.0 : 0.0;
        ctx.nodes[*state_id].params["legato_id"] =
            static_cast<double>(*chain_id);
    } else if (piece.open_passthrough) {
        ctx.pending_legato_boundaries.push_back(PendingLegatoBoundary{
            .passthrough_id = piece.entry_id,
            .has_after = has_after,
            .chain_id = *chain_id,
            .line = term.line,
            .column = term.column,
        });
    } else {
        throw ResolveException("'~' cannot connect a term that forks (&)",
                               term.line, term.column);
    }

    if (!has_after) chain_id.reset();
}

auto expand_comp_expr(const CompExpr &comp, ExpansionContext &ctx) -> Piece {
    if (comp.terms.empty()) throw ResolveException("empty sequence", 0, 0);

    auto first = expand_term(comp.terms.front(), ctx);
    if (comp.terms.size() == 1) return first;

    std::optional<size_t> legato_chain_id;
    apply_legato(ctx, first, comp.terms.front(),
                 comp.terms.front().legato_after,
                 comp.terms.front().legato_after, legato_chain_id);

    const auto members = make_members();
    merge_members(members, first.members);
    auto prev = first;
    for (size_t i = 1; i < comp.terms.size(); ++i) {
        const auto next = expand_term(comp.terms[i], ctx);
        apply_legato(ctx, next, comp.terms[i],
                     comp.terms[i - 1].legato_after ||
                         comp.terms[i].legato_after,
                     comp.terms[i].legato_after, legato_chain_id);
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

auto make_predefined_decls(size_t chromatic_scale_index)
    -> std::vector<VarDecl> {
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
            const auto semitone = degree + offset;
            const auto freq = 440.0 * std::pow(2.0, semitone / 12.0);
            const auto chromatic_degree = ((semitone % 12) + 12) % 12;

            VarDecl decl;
            decl.name =
                std::string(1, static_cast<char>(std::toupper(letter))) +
                suffix;
            decl.kind = VarDecl::Kind::BlockDef;
            decl.block.params.push_back(
                Param{.name = "freq", .value = make_expr(freq)});
            decl.block.params.push_back(
                Param{.name = "dur", .value = make_expr(1)});
            decl.block.params.push_back(
                Param{.name = "degree", .value = make_expr(chromatic_degree)});
            decl.block.params.push_back(
                Param{.name = "octave", .value = make_expr(4)});
            decl.block.params.push_back(Param{
                .name = "scale",
                .value =
                    make_expr(static_cast<double>(chromatic_scale_index))});
            decls.push_back(std::move(decl));
        }
    }
    return decls;
}

auto make_predefined_scales()
    -> std::vector<std::pair<std::string, std::vector<double>>> {
    const std::vector<std::pair<std::string, std::vector<int>>> specs = {
        {"chromatic", {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}},
        {"major", {0, 2, 4, 5, 7, 9, 11}},
        {"minor", {0, 2, 3, 5, 7, 8, 10}},
        {"harmonic_minor", {0, 2, 3, 5, 7, 8, 11}},
        {"melodic_minor", {0, 2, 3, 5, 7, 9, 11}},
    };
    std::vector<std::pair<std::string, std::vector<double>>> result;
    result.reserve(specs.size());
    for (const auto &[name, steps] : specs) {
        std::vector<double> values;
        values.reserve(steps.size());
        for (const auto step : steps)
            values.push_back(std::pow(2.0, step / 12.0));
        result.emplace_back(name, std::move(values));
    }
    return result;
}

auto decl_is_atomic(
    const VarDecl &decl,
    const std::unordered_map<std::string, const VarDecl *> &symbols,
    std::unordered_set<std::string> &visiting) -> bool {
    if (decl.kind == VarDecl::Kind::BlockDef) return true;
    if (decl.kind != VarDecl::Kind::CompDef) return false;
    if (decl.comp.terms.size() != 1) return false;
    const auto &term = decl.comp.terms[0];
    if (term.kind == Term::Kind::AtomicJoin)
        return term.lhs_expr->terms.size() == 1 &&
               term.lhs_expr->terms[0].kind == Term::Kind::VarRef;
    if (term.kind != Term::Kind::VarRef) return false;
    if (visiting.contains(term.var_name)) return false;
    const auto it = symbols.find(term.var_name);
    if (it == symbols.end()) return false;
    visiting.insert(term.var_name);
    const auto result = decl_is_atomic(*it->second, symbols, visiting);
    visiting.erase(term.var_name);
    return result;
}

auto resolve_passthroughs(ExpansionContext &ctx) -> void {
    for (const auto &passthrough : ctx.passthroughs) {
        auto from = passthrough->id;
        if (passthrough->head_id.has_value()) {
            ctx.nodes[from].next.push_back(*passthrough->head_id);
            from = passthrough->tail_id;
        }
        ctx.nodes[from].next.push_back(*passthrough->pending_target);
    }
}

auto resolve_pending_legato_boundaries(ExpansionContext &ctx) -> void {
    for (const auto &pending : ctx.pending_legato_boundaries) {
        if (ctx.nodes[pending.passthrough_id].next.empty())
            throw ResolveException("'~' cannot connect to a loop that never "
                                   "plays a note",
                                   pending.line, pending.column);
        const auto target = ctx.nodes[pending.passthrough_id].next[0];
        const auto state_id = find_first_state(ctx, target);
        if (!state_id)
            throw ResolveException("'~' cannot connect a term that forks (&)",
                                   pending.line, pending.column);
        ctx.nodes[*state_id].params["legato"] = pending.has_after ? 1.0 : 0.0;
        ctx.nodes[*state_id].params["legato_id"] =
            static_cast<double>(pending.chain_id);
    }
}

} // namespace

auto prune_unreachable(ExpandedGraph &graph) -> void {
    std::vector<bool> reachable(graph.nodes.size(), false);
    std::vector<size_t> stack;
    for (const auto &machine : graph.entries)
        for (auto id : machine) stack.push_back(id);
    while (!stack.empty()) {
        const auto id = stack.back();
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

namespace {

auto remap_legato_ids(ExpansionContext &ctx,
                      const std::unordered_map<size_t, size_t> &clone_of)
    -> void {
    std::unordered_map<double, size_t> fresh_id;
    for (const auto &[original, clone_id] : clone_of) {
        const auto it = ctx.nodes[clone_id].params.find("legato_id");
        if (it == ctx.nodes[clone_id].params.end()) continue;
        const auto [fresh_it, inserted] =
            fresh_id.try_emplace(it->second, ctx.next_legato_id);
        if (inserted) ++ctx.next_legato_id;
        it->second = static_cast<double>(fresh_it->second);
    }
}

auto resolve_pending_reverse_clones(ExpansionContext &ctx) -> void {
    std::unordered_map<size_t, size_t> resolved_bridges;

    for (const auto &pr : ctx.pending_reverse_clones) {
        const auto all_members =
            std::make_shared<std::unordered_set<size_t>>(*pr.members);
        for (auto original : *pr.members) {
            const auto bridge_it = resolved_bridges.find(original);
            if (bridge_it == resolved_bridges.end()) continue;
            const auto inner_exit = bridge_it->second;
            if (!pr.members->contains(inner_exit)) continue;
            auto cur = original;
            while (cur != inner_exit && !ctx.nodes[cur].next.empty()) {
                const auto nxt = ctx.nodes[cur].next[0];
                if (nxt == inner_exit) break;
                if (!all_members->insert(nxt).second) break;
                if (pr.ring_members) pr.ring_members->insert(nxt);
                cur = nxt;
            }
        }

        std::unordered_map<size_t, size_t> clone_of;
        auto get_clone = [&](size_t original) -> size_t {
            const auto it = clone_of.find(original);
            if (it != clone_of.end()) return it->second;
            const auto clone_id = ctx.nodes.size();
            auto clone = ctx.nodes[original];
            clone.id = clone_id;
            clone.next.clear();
            ctx.nodes.push_back(std::move(clone));
            clone_of[original] = clone_id;
            return clone_id;
        };

        for (const auto original : *all_members) get_clone(original);
        remap_legato_ids(ctx, clone_of);
        for (const auto original : *all_members) {
            const auto clone_id = clone_of[original];
            for (auto succ : ctx.nodes[original].next) {
                if (!clone_of.contains(succ)) continue;
                auto flip = true;
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
            const auto pop_it = ctx.transform_pop_of_push.find(original);
            if (pop_it == ctx.transform_pop_of_push.end()) continue;
            const auto original_pop = pop_it->second;
            if (!all_members->contains(original_pop)) continue;

            const auto push_clone = clone_of[original];
            const auto pop_clone = clone_of[original_pop];

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

auto resolve_pending_repeat_clones(ExpansionContext &ctx) -> void {
    for (const auto &pr : ctx.pending_repeat_clones) {
        auto prev_exit = pr.entry_anchor;

        for (size_t copy = 0; copy < pr.count; ++copy) {
            std::unordered_map<size_t, size_t> clone_of;
            const auto get_clone = [&](size_t original) -> size_t {
                auto it = clone_of.find(original);
                if (it != clone_of.end()) return it->second;
                const auto clone_id = ctx.nodes.size();
                GraphNode clone = ctx.nodes[original];
                clone.id = clone_id;
                clone.next.clear();
                ctx.nodes.push_back(std::move(clone));
                clone_of[original] = clone_id;
                return clone_id;
            };

            for (auto original : *pr.members) get_clone(original);
            remap_legato_ids(ctx, clone_of);
            for (auto original : *pr.members) {
                const auto clone_id = clone_of[original];
                for (auto succ : ctx.nodes[original].next) {
                    if (!clone_of.contains(succ)) continue;
                    ctx.nodes[clone_id].next.push_back(clone_of[succ]);
                }
            }

            ctx.nodes[prev_exit].next.push_back(clone_of[pr.body_entry_id]);
            prev_exit = clone_of[pr.body_exit_id];
        }

        ctx.nodes[prev_exit].next.push_back(pr.exit_anchor);
    }
}

} // namespace

auto expand_program(const Program &program) -> ExpandedGraph {
    ExpansionContext ctx;

    ExpandedGraph graph;
    for (auto &[name, values] : make_predefined_scales()) {
        ctx.scale_index[name] = graph.scales.size();
        graph.scales.emplace_back(name, std::move(values));
    }

    const auto predefined_decls =
        make_predefined_decls(ctx.scale_index.at("chromatic"));
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

    for (const auto &decl : program.decls) {
        if (decl.kind != VarDecl::Kind::ScaleDef) continue;
        std::vector<double> values;
        values.reserve(decl.scale.size());
        for (const auto &elem : decl.scale) values.push_back(fold_expr(*elem));
        std::ranges::sort(values);
        values.erase(std::ranges::unique(values).begin(), values.end());
        const auto existing_it = ctx.scale_index.find(decl.name);
        if (existing_it != ctx.scale_index.end()) {
            graph.scales[existing_it->second] = {decl.name, std::move(values)};
            continue;
        }
        ctx.scale_index[decl.name] = graph.scales.size();
        graph.scales.emplace_back(decl.name, std::move(values));
    }

    for (const auto &play : program.plays) {
        for (const auto &machine : play.machines) {
            const auto piece = expand_comp_expr(machine, ctx);
            graph.entries.push_back({piece.entry_id});
        }
    }
    resolve_passthroughs(ctx);
    resolve_pending_legato_boundaries(ctx);
    resolve_pending_reverse_clones(ctx);
    resolve_pending_repeat_clones(ctx);
    graph.nodes = std::move(ctx.nodes);
    graph.owned_exprs = std::move(ctx.owned_exprs);
    optimize_graph(graph);
    return graph;
}
