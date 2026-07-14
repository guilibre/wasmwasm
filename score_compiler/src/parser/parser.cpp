#include "parser.hpp"

#include <array>
#include <utility>

namespace {

auto make_number(double value) -> std::unique_ptr<Expr> {
    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Number;
    expr->number = value;
    expr->number_rational =
        Rational::from_decimal_literal(std::to_string(value));
    return expr;
}

auto make_number_literal(const std::string &lexeme) -> std::unique_ptr<Expr> {
    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Number;
    expr->number_rational = Rational::from_decimal_literal(lexeme);
    expr->number = expr->number_rational.to_double();
    return expr;
}

auto make_binary(BinOp op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    -> std::unique_ptr<Expr> {
    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Binary;
    expr->op = op;
    expr->lhs = std::move(lhs);
    expr->rhs = std::move(rhs);
    return expr;
}

auto wrap_as_comp_expr(Term term) -> std::unique_ptr<CompExpr> {
    auto comp = std::make_unique<CompExpr>();
    comp->terms.push_back(std::move(term));
    return comp;
}

auto wrap_terms_as_comp_expr(std::vector<Term> terms)
    -> std::unique_ptr<CompExpr> {
    auto comp = std::make_unique<CompExpr>();
    comp->terms = std::move(terms);
    return comp;
}

auto wrap_terms_as_single_term(std::vector<Term> terms) -> Term {
    if (terms.size() == 1) return std::move(terms.front());
    Term fork;
    fork.kind = Term::Kind::Fork;
    fork.line = terms.front().line;
    fork.column = terms.front().column;
    fork.branches.push_back(wrap_terms_as_comp_expr(std::move(terms)));
    return fork;
}

} // namespace

Parser::Parser(Tokenizer tokenizer) : tokenizer(tokenizer) { advance(); }

void Parser::advance() { current = this->tokenizer.next(); }

auto Parser::match(TokenKind kind) -> bool {
    if (current.kind != kind) return false;
    advance();
    return true;
}

auto Parser::expect(TokenKind kind, const std::string &what) -> Token {
    if (current.kind != kind)
        throw ParseException("expected " + what + ", got '" + current.lexeme +
                                 "'",
                             current.line, current.column);
    const auto tok = current;
    advance();
    return tok;
}

void Parser::end_statement() {
    if (current.kind == TokenKind::Eof || current.kind == TokenKind::RBrace)
        return;
    expect(TokenKind::Semicolon, "';'");
}

auto Parser::starts_term() const -> bool {
    return current.kind == TokenKind::Ident ||
           current.kind == TokenKind::LParen ||
           current.kind == TokenKind::LBrace ||
           current.kind == TokenKind::KwChoose ||
           current.kind == TokenKind::KwEmit;
}

auto Parser::parse_factor() -> std::unique_ptr<Expr> {
    if (match(TokenKind::LParen)) {
        auto expr = parse_ternary();
        expect(TokenKind::RParen, "')'");
        return expr;
    }
    if (current.kind == TokenKind::LBracket) {
        auto expr = std::make_unique<Expr>();
        expr->kind = Expr::Kind::Array;
        expr->elements = parse_array_literal();
        return expr;
    }
    if (current.kind == TokenKind::Minus) {
        advance();
        auto inner = parse_factor();
        return make_binary(BinOp::Sub, make_number(0), std::move(inner));
    }
    if (current.kind == TokenKind::KwNull) {
        advance();
        auto expr = std::make_unique<Expr>();
        expr->kind = Expr::Kind::Null;
        return expr;
    }
    if (current.kind == TokenKind::String) {
        const auto tok = current;
        advance();
        auto expr = std::make_unique<Expr>();
        expr->kind = Expr::Kind::String;
        expr->string_value = tok.lexeme;
        expr->line = tok.line;
        expr->column = tok.column;
        return expr;
    }
    if (current.kind == TokenKind::Ident) {
        const auto tok = current;
        advance();
        if (tok.lexeme == "true" || tok.lexeme == "false")
            return make_number(tok.lexeme == "true" ? 1.0 : 0.0);
        auto expr = std::make_unique<Expr>();
        expr->kind = Expr::Kind::Ident;
        expr->ident_name = tok.lexeme;
        expr->line = tok.line;
        expr->column = tok.column;
        return expr;
    }
    const auto &tok = expect(TokenKind::Number, "number");
    return make_number_literal(tok.lexeme);
}

auto Parser::parse_comparison() -> std::unique_ptr<Expr> {
    auto lhs = parse_expr();
    static constexpr std::array<std::pair<TokenKind, BinOp>, 6> kOps = {{
        {TokenKind::EqEq, BinOp::Eq},
        {TokenKind::NotEq, BinOp::NotEq},
        {TokenKind::LessEq, BinOp::LtEq},
        {TokenKind::GreaterEq, BinOp::GtEq},
        {TokenKind::Less, BinOp::Lt},
        {TokenKind::Greater, BinOp::Gt},
    }};
    for (const auto &[kind, op] : kOps) {
        if (current.kind == kind) {
            advance();
            auto rhs = parse_expr();
            return make_binary(op, std::move(lhs), std::move(rhs));
        }
    }
    return lhs;
}

auto Parser::parse_logic_and() -> std::unique_ptr<Expr> {
    auto lhs = parse_comparison();
    while (current.kind == TokenKind::Ampersand) {
        advance();
        auto rhs = parse_comparison();
        lhs = make_binary(BinOp::And, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

auto Parser::parse_logic_or() -> std::unique_ptr<Expr> {
    auto lhs = parse_logic_and();
    while (current.kind == TokenKind::Or) {
        advance();
        auto rhs = parse_logic_and();
        lhs = make_binary(BinOp::Or, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

auto Parser::parse_ternary() -> std::unique_ptr<Expr> {
    auto cond = parse_logic_or();
    if (current.kind != TokenKind::Question) return cond;
    advance();
    auto then_expr = parse_ternary();
    expect(TokenKind::Colon, "':'");
    auto else_expr = parse_ternary();

    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Ternary;
    expr->ternary_cond = std::move(cond);
    expr->ternary_then = std::move(then_expr);
    expr->ternary_else = std::move(else_expr);
    return expr;
}

auto Parser::parse_array_literal() -> std::vector<std::unique_ptr<Expr>> {
    std::vector<std::unique_ptr<Expr>> elements;
    expect(TokenKind::LBracket, "'['");
    if (current.kind != TokenKind::RBracket) {
        elements.push_back(parse_ternary());
        while (match(TokenKind::Comma)) elements.push_back(parse_ternary());
    }
    expect(TokenKind::RBracket, "']'");
    return elements;
}

auto Parser::parse_pow() -> std::unique_ptr<Expr> {
    auto lhs = parse_factor();
    if (current.kind == TokenKind::Caret) {
        advance();
        auto rhs = parse_pow();
        return make_binary(BinOp::Pow, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

auto Parser::parse_arith_term() -> std::unique_ptr<Expr> {
    auto lhs = parse_pow();
    while (current.kind == TokenKind::Star ||
           current.kind == TokenKind::Slash ||
           current.kind == TokenKind::Percent) {
        const auto op = current.kind == TokenKind::Star    ? BinOp::Mul
                        : current.kind == TokenKind::Slash ? BinOp::Div
                                                           : BinOp::Mod;
        advance();
        auto rhs = parse_pow();
        lhs = make_binary(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

auto Parser::parse_expr() -> std::unique_ptr<Expr> {
    auto lhs = parse_arith_term();
    while (current.kind == TokenKind::Plus ||
           current.kind == TokenKind::Minus) {
        const auto op =
            current.kind == TokenKind::Plus ? BinOp::Add : BinOp::Sub;
        advance();
        auto rhs = parse_arith_term();
        lhs = make_binary(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

auto Parser::parse_block() -> Block {
    Block block;
    block.line = current.line;
    block.column = current.column;
    expect(TokenKind::LBrace, "'{'");
    while (current.kind != TokenKind::RBrace) {
        const auto is_const = match(TokenKind::KwConst);
        const auto &name = expect(TokenKind::Ident, "parameter name");
        expect(TokenKind::Colon, "':'");
        Param param;
        param.name = name.lexeme;
        param.value = parse_ternary();
        param.is_const = is_const;
        block.params.push_back(std::move(param));
        end_statement();
    }
    expect(TokenKind::RBrace, "'}'");
    return block;
}

auto Parser::parse_comp_atom() -> std::vector<Term> {
    if (current.kind == TokenKind::LParen) {
        const auto line = current.line;
        const auto column = current.column;
        advance();
        auto inner = parse_comp_expr();
        expect(TokenKind::RParen, "')'");

        if (current.kind == TokenKind::At) {
            std::vector<Term> result;
            result.push_back(continue_atomic_join_comp(
                std::make_unique<CompExpr>(std::move(inner))));
            return result;
        }
        if (current.kind == TokenKind::Pipe) {
            std::vector<Term> result;
            result.push_back(parse_pipe_suffix(
                std::make_unique<CompExpr>(std::move(inner))));
            return result;
        }
        if (current.kind == TokenKind::Bang) {
            std::vector<Term> result;
            result.push_back(parse_bang_suffix(
                std::make_unique<CompExpr>(std::move(inner))));
            return result;
        }

        if (inner.terms.size() == 1) {
            std::vector<Term> result;
            result.push_back(
                continue_octave_suffix(std::move(inner.terms.front())));
            return result;
        }

        if (current.kind == TokenKind::Tick ||
            current.kind == TokenKind::Comma ||
            current.kind == TokenKind::Ampersand) {
            Term fork;
            fork.kind = Term::Kind::Fork;
            fork.line = line;
            fork.column = column;
            fork.branches.push_back(
                std::make_unique<CompExpr>(std::move(inner)));
            std::vector<Term> result;
            result.push_back(continue_octave_suffix(std::move(fork)));
            return result;
        }

        std::vector<Term> result;
        result.reserve(inner.terms.size());
        for (auto &term : inner.terms) result.push_back(std::move(term));
        return result;
    }

    Term term;
    term.line = current.line;
    term.column = current.column;
    if (current.kind == TokenKind::LBrace) {
        term.kind = Term::Kind::BlockLit;
        term.block_lit = parse_block();
    } else if (match(TokenKind::KwChoose)) {
        term.kind = Term::Kind::Choose;
        term.pipe_expr = parse_ternary();
        term.branches.push_back(wrap_terms_as_comp_expr(parse_fork_term()));
        term.branches.push_back(wrap_terms_as_comp_expr(parse_fork_term()));
    } else if (match(TokenKind::KwEmit)) {
        term.kind = Term::Kind::Emit;
        term.rhs_name = expect(TokenKind::String, "signal id").lexeme;
        term.block_lit = parse_block();
    } else {
        const auto &name = expect(TokenKind::Ident, "variable name");
        term.kind = Term::Kind::VarRef;
        term.var_name = name.lexeme;
    }

    std::vector<Term> result;
    result.push_back(continue_octave_suffix(std::move(term)));
    return result;
}

auto Parser::continue_octave_suffix(Term term) -> Term {
    while (current.kind == TokenKind::Tick ||
           current.kind == TokenKind::Comma) {
        const auto is_up = current.kind == TokenKind::Tick;
        advance();

        const auto make_ident =
            [](const std::string &name) -> std::unique_ptr<Expr> {
            auto ident = std::make_unique<Expr>();
            ident->kind = Expr::Kind::Ident;
            ident->ident_name = name;
            return ident;
        };

        const auto make_guarded_join = [&](const std::string &param_name,
                                           std::unique_ptr<Expr> new_value,
                                           Term inner) -> Term {
            Term join;
            join.kind = Term::Kind::AtomicJoin;
            join.line = inner.line;
            join.column = inner.column;
            join.rhs_is_block = true;

            auto guarded = std::make_unique<Expr>();
            guarded->kind = Expr::Kind::Ternary;
            guarded->ternary_cond = make_ident(param_name);
            guarded->ternary_then = std::move(new_value);
            guarded->ternary_else = std::make_unique<Expr>();
            guarded->ternary_else->kind = Expr::Kind::Null;

            Param param;
            param.name = param_name;
            param.value = std::move(guarded);
            join.rhs_block.params.push_back(std::move(param));

            join.lhs_expr = wrap_as_comp_expr(std::move(inner));
            return join;
        };

        auto freq_value = make_binary(BinOp::Mul, make_ident("freq"),
                                      make_number(is_up ? 2.0 : 0.5));
        term =
            make_guarded_join("freq", std::move(freq_value), std::move(term));

        auto octave_value = make_binary(is_up ? BinOp::Add : BinOp::Sub,
                                        make_ident("octave"), make_number(1.0));
        term = make_guarded_join("octave", std::move(octave_value),
                                 std::move(term));
    }

    return term;
}

auto Parser::parse_atomic_join() -> std::vector<Term> {
    return parse_comp_atom();
}

auto Parser::continue_atomic_join_comp(std::unique_ptr<CompExpr> lhs) -> Term {
    const auto line = lhs->terms.front().line;
    const auto column = lhs->terms.front().column;
    Term join;
    join.kind = Term::Kind::AtomicJoin;
    join.line = line;
    join.column = column;
    join.lhs_expr = std::move(lhs);
    advance();
    if (current.kind == TokenKind::LBrace) {
        join.rhs_is_block = true;
        join.rhs_block = parse_block();
    } else {
        const auto &rhs_name = expect(TokenKind::Ident, "variable name");
        join.rhs_name = rhs_name.lexeme;
    }
    return continue_atomic_join(std::move(join));
}

auto Parser::continue_atomic_join(Term lhs) -> Term {
    while (current.kind == TokenKind::At) {
        advance();
        Term join;
        join.kind = Term::Kind::AtomicJoin;
        join.line = lhs.line;
        join.column = lhs.column;
        join.lhs_expr = wrap_as_comp_expr(std::move(lhs));
        if (current.kind == TokenKind::LBrace) {
            join.rhs_is_block = true;
            join.rhs_block = parse_block();
        } else {
            const auto &rhs_name = expect(TokenKind::Ident, "variable name");
            join.rhs_name = rhs_name.lexeme;
        }
        lhs = std::move(join);
    }
    return lhs;
}

auto Parser::parse_pipe_suffix(std::unique_ptr<CompExpr> lhs) -> Term {
    const auto line = lhs->terms.front().line;
    const auto column = lhs->terms.front().column;
    advance();

    Term pipe;
    pipe.kind = Term::Kind::Pipe;
    pipe.line = line;
    pipe.column = column;
    pipe.lhs_expr = std::move(lhs);

    if (match(TokenKind::KwReverse)) {
        pipe.pipe_op = Term::PipeOp::Reverse;
        return pipe;
    }

    if (match(TokenKind::KwListen)) {
        pipe.pipe_op = Term::PipeOp::Listen;
        pipe.rhs_name = expect(TokenKind::String, "signal id").lexeme;
        return pipe;
    }

    expect(TokenKind::KwRepeat, "'reverse', 'listen', or 'repeat'");
    pipe.pipe_op = Term::PipeOp::Repeat;
    pipe.pipe_expr = parse_ternary();
    return pipe;
}

auto Parser::parse_bang_suffix(std::unique_ptr<CompExpr> lhs) -> Term {
    const auto line = lhs->terms.front().line;
    const auto column = lhs->terms.front().column;
    advance();
    auto expr = parse_ternary();

    Term join;
    join.kind = Term::Kind::AtomicJoin;
    join.line = line;
    join.column = column;
    join.lhs_expr = std::move(lhs);
    join.rhs_is_block = true;

    Param param;
    param.name = "dur";
    param.value = std::move(expr);
    join.rhs_block.params.push_back(std::move(param));
    return join;
}

auto Parser::continue_pipe_term(std::vector<Term> terms) -> std::vector<Term> {
    while (current.kind == TokenKind::Pipe || current.kind == TokenKind::Bang ||
           current.kind == TokenKind::At) {
        Term applied;
        if (current.kind == TokenKind::At) {
            applied = continue_atomic_join_comp(
                wrap_terms_as_comp_expr(std::move(terms)));
        } else if (current.kind == TokenKind::Pipe) {
            applied =
                parse_pipe_suffix(wrap_terms_as_comp_expr(std::move(terms)));
        } else {
            applied =
                parse_bang_suffix(wrap_terms_as_comp_expr(std::move(terms)));
        }
        terms.clear();
        terms.push_back(std::move(applied));
    }
    return terms;
}

auto Parser::parse_pipe_term() -> Term {
    return wrap_terms_as_single_term(continue_pipe_term(parse_atomic_join()));
}

auto Parser::continue_fork_term(Term first) -> Term {
    if (current.kind != TokenKind::Ampersand) return first;
    Term fork;
    fork.kind = Term::Kind::Fork;
    fork.line = first.line;
    fork.column = first.column;
    fork.branches.push_back(wrap_as_comp_expr(std::move(first)));
    while (match(TokenKind::Ampersand))
        fork.branches.push_back(wrap_as_comp_expr(parse_pipe_term()));
    return fork;
}

auto Parser::parse_fork_term() -> std::vector<Term> {
    auto terms = continue_pipe_term(parse_atomic_join());
    if (terms.size() != 1) {
        if (current.kind != TokenKind::Ampersand) return terms;
        auto merged = wrap_terms_as_single_term(std::move(terms));
        terms.clear();
        terms.push_back(std::move(merged));
    }

    std::vector<Term> result;
    result.push_back(continue_fork_term(std::move(terms.front())));
    return result;
}

void Parser::consume_legato_tilde(Term &term) {
    if (!match(TokenKind::Tilde)) return;
    term.legato_after = true;
    if (!starts_term())
        throw ParseException("expected a note after '~'", current.line,
                             current.column);
}

void Parser::parse_comp_terms(CompExpr &comp) {
    while (starts_term()) {
        auto terms = parse_fork_term();
        for (auto &term : terms) comp.terms.push_back(std::move(term));
        consume_legato_tilde(comp.terms.back());
    }
}

auto Parser::parse_comp_expr() -> CompExpr {
    CompExpr comp;
    parse_comp_terms(comp);
    return comp;
}

auto Parser::parse_var_decl() -> VarDecl {
    VarDecl decl;
    decl.line = current.line;
    decl.column = current.column;
    const auto &name = expect(TokenKind::Ident, "variable name");
    decl.name = name.lexeme;
    expect(TokenKind::Equals, "'='");
    if (current.kind == TokenKind::LBrace) {
        const auto block_line = current.line;
        const auto block_column = current.column;
        Block block = parse_block();
        if (current.kind == TokenKind::Semicolon ||
            current.kind == TokenKind::Eof ||
            current.kind == TokenKind::RBrace) {
            decl.kind = VarDecl::Kind::BlockDef;
            decl.block = std::move(block);
        } else {
            decl.kind = VarDecl::Kind::CompDef;
            Term term;
            term.kind = Term::Kind::BlockLit;
            term.block_lit = std::move(block);
            term.line = block_line;
            term.column = block_column;
            auto joined =
                continue_atomic_join(continue_octave_suffix(std::move(term)));
            std::vector<Term> single;
            single.push_back(std::move(joined));
            Term first = continue_fork_term(wrap_terms_as_single_term(
                continue_pipe_term(std::move(single))));
            decl.comp.terms.push_back(std::move(first));
            consume_legato_tilde(decl.comp.terms.back());
            parse_comp_terms(decl.comp);
        }
    } else if (current.kind == TokenKind::LBracket) {
        decl.kind = VarDecl::Kind::ScaleDef;
        decl.scale = parse_array_literal();
    } else {
        decl.kind = VarDecl::Kind::CompDef;
        decl.comp = parse_comp_expr();
    }
    end_statement();
    return decl;
}

auto Parser::parse_play_stmt() -> PlayStmt {
    PlayStmt stmt;
    stmt.line = current.line;
    stmt.column = current.column;
    expect(TokenKind::KwPlay, "'play'");
    stmt.machines.push_back(parse_comp_expr());
    end_statement();
    return stmt;
}

auto Parser::parse() -> Program {
    Program program;
    while (current.kind != TokenKind::Eof) {
        if (current.kind == TokenKind::KwPlay)
            program.plays.push_back(parse_play_stmt());
        else
            program.decls.push_back(parse_var_decl());
    }
    return program;
}
