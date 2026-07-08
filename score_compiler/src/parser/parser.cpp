#include "parser.hpp"

#include <array>
#include <utility>

namespace {

auto make_number(double value) -> std::unique_ptr<Expr> {
    auto expr = std::make_unique<Expr>();
    expr->kind = Expr::Kind::Number;
    expr->number = value;
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
    Token tok = current;
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
           current.kind == TokenKind::LBrace;
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
    if (current.kind == TokenKind::Ident) {
        Token tok = current;
        advance();
        auto expr = std::make_unique<Expr>();
        expr->kind = Expr::Kind::Ident;
        expr->ident_name = tok.lexeme;
        expr->line = tok.line;
        expr->column = tok.column;
        return expr;
    }
    Token tok = expect(TokenKind::Number, "number");
    return make_number(std::stod(tok.lexeme));
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

auto Parser::parse_ternary() -> std::unique_ptr<Expr> {
    auto cond = parse_comparison();
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
           current.kind == TokenKind::Slash) {
        BinOp op = current.kind == TokenKind::Star ? BinOp::Mul : BinOp::Div;
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
        BinOp op = current.kind == TokenKind::Plus ? BinOp::Add : BinOp::Sub;
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
        Token name = expect(TokenKind::Ident, "parameter name");
        expect(TokenKind::Colon, "':'");
        if (name.lexeme == "instrument") {
            if (block.instrument.has_value())
                throw ParseException("duplicate 'instrument' field", name.line,
                                     name.column);
            Token value = expect(TokenKind::String, "string literal");
            block.instrument = value.lexeme;
        } else {
            Param param;
            param.name = name.lexeme;
            param.value = parse_ternary();
            block.params.push_back(std::move(param));
        }
        end_statement();
    }
    expect(TokenKind::RBrace, "'}'");
    return block;
}

auto Parser::parse_comp_atom() -> Term {
    Term term;
    term.line = current.line;
    term.column = current.column;
    if (match(TokenKind::LParen)) {
        term.kind = Term::Kind::Fork;
        term.branches.push_back(std::make_unique<CompExpr>(parse_comp_expr()));
        expect(TokenKind::RParen, "')'");
    } else if (current.kind == TokenKind::LBrace) {
        term.kind = Term::Kind::BlockLit;
        term.block_lit = parse_block();
    } else {
        Token name = expect(TokenKind::Ident, "variable name");
        term.kind = Term::Kind::VarRef;
        term.var_name = name.lexeme;
    }

    while (current.kind == TokenKind::Tick ||
           current.kind == TokenKind::Comma) {
        bool is_up = current.kind == TokenKind::Tick;
        advance();
        Term pipe;
        pipe.kind = Term::Kind::Pipe;
        pipe.pipe_op = Term::PipeOp::Transform;
        pipe.line = term.line;
        pipe.column = term.column;
        pipe.pipe_param_name = "freq";

        auto make_freq_ident = []() -> std::unique_ptr<Expr> {
            auto ident = std::make_unique<Expr>();
            ident->kind = Expr::Kind::Ident;
            ident->ident_name = "freq";
            return ident;
        };

        auto scaled = make_binary(BinOp::Mul, make_freq_ident(),
                                  make_number(is_up ? 2.0 : 0.5));

        auto guarded = std::make_unique<Expr>();
        guarded->kind = Expr::Kind::Ternary;
        guarded->ternary_cond = make_freq_ident();
        guarded->ternary_then = std::move(scaled);
        guarded->ternary_else = std::make_unique<Expr>();
        guarded->ternary_else->kind = Expr::Kind::Null;

        pipe.pipe_expr = std::move(guarded);
        pipe.lhs_expr = wrap_as_comp_expr(std::move(term));
        term = std::move(pipe);
    }

    return term;
}

auto Parser::parse_atomic_join() -> Term {
    Term lhs = parse_comp_atom();
    if (current.kind != TokenKind::At) return lhs;
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
            Token rhs_name = expect(TokenKind::Ident, "variable name");
            join.rhs_name = rhs_name.lexeme;
        }
        lhs = std::move(join);
    }
    return lhs;
}

auto Parser::parse_pipe_suffix(std::unique_ptr<CompExpr> lhs) -> Term {
    size_t line = lhs->terms.front().line;
    size_t column = lhs->terms.front().column;
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

    expect(TokenKind::KwTransform, "'transform'");
    Token param = expect(TokenKind::Ident, "parameter name");
    expect(TokenKind::KwBy, "'by'");
    auto expr = parse_ternary();

    pipe.pipe_op = Term::PipeOp::Transform;
    pipe.pipe_param_name = param.lexeme;
    pipe.pipe_expr = std::move(expr);
    return pipe;
}

auto Parser::parse_bang_suffix(std::unique_ptr<CompExpr> lhs) -> Term {
    size_t line = lhs->terms.front().line;
    size_t column = lhs->terms.front().column;
    advance();
    auto expr = parse_ternary();

    Term pipe;
    pipe.kind = Term::Kind::Pipe;
    pipe.pipe_op = Term::PipeOp::Transform;
    pipe.line = line;
    pipe.column = column;
    pipe.lhs_expr = std::move(lhs);
    pipe.pipe_param_name = "dur";
    pipe.pipe_expr = std::move(expr);
    return pipe;
}

auto Parser::continue_pipe_term(Term term) -> Term {
    while (current.kind == TokenKind::Pipe || current.kind == TokenKind::Bang) {
        term = current.kind == TokenKind::Pipe
                   ? parse_pipe_suffix(wrap_as_comp_expr(std::move(term)))
                   : parse_bang_suffix(wrap_as_comp_expr(std::move(term)));
    }
    return term;
}

auto Parser::parse_pipe_term() -> Term {
    return continue_pipe_term(parse_atomic_join());
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

auto Parser::parse_fork_term() -> Term {
    return continue_fork_term(parse_pipe_term());
}

auto Parser::parse_comp_expr() -> CompExpr {
    CompExpr comp;
    while (starts_term()) comp.terms.push_back(parse_fork_term());
    return comp;
}

auto Parser::parse_var_decl() -> VarDecl {
    VarDecl decl;
    decl.line = current.line;
    decl.column = current.column;
    Token name = expect(TokenKind::Ident, "variable name");
    decl.name = name.lexeme;
    expect(TokenKind::Equals, "'='");
    if (current.kind == TokenKind::LBrace) {
        size_t block_line = current.line;
        size_t block_column = current.column;
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
            Term first =
                continue_fork_term(continue_pipe_term(std::move(term)));
            decl.comp.terms.push_back(std::move(first));
            while (starts_term()) decl.comp.terms.push_back(parse_fork_term());
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
