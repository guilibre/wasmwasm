#include "parser.hpp"

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

void Parser::skip_newlines() {
    while (current.kind == TokenKind::Newline) advance();
}

void Parser::end_statement() {
    if (current.kind == TokenKind::Eof) return;
    expect(TokenKind::Newline, "end of line");
    skip_newlines();
}

auto Parser::starts_term() const -> bool {
    return current.kind == TokenKind::Ident ||
           current.kind == TokenKind::LParen;
}

auto Parser::parse_factor() -> std::unique_ptr<Expr> {
    if (match(TokenKind::LParen)) {
        auto expr = parse_expr();
        expect(TokenKind::RParen, "')'");
        return expr;
    }
    if (current.kind == TokenKind::Minus) {
        advance();
        auto inner = parse_factor();
        return make_binary(BinOp::Sub, make_number(0), std::move(inner));
    }
    Token tok = expect(TokenKind::Number, "number");
    return make_number(std::stod(tok.lexeme));
}

auto Parser::parse_arith_term() -> std::unique_ptr<Expr> {
    auto lhs = parse_factor();
    while (current.kind == TokenKind::Star ||
           current.kind == TokenKind::Slash) {
        BinOp op = current.kind == TokenKind::Star ? BinOp::Mul : BinOp::Div;
        advance();
        auto rhs = parse_factor();
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
    skip_newlines();
    while (current.kind != TokenKind::RBrace) {
        Param param;
        Token name = expect(TokenKind::Ident, "parameter name");
        param.name = name.lexeme;
        expect(TokenKind::Colon, "':'");
        param.value = parse_expr();
        block.params.push_back(std::move(param));
        end_statement();
    }
    expect(TokenKind::RBrace, "'}'");
    return block;
}

auto Parser::parse_comp_term() -> Term {
    Term term;
    term.line = current.line;
    term.column = current.column;
    if (current.kind == TokenKind::LParen) {
        advance();
        term = parse_fork_group();
        expect(TokenKind::RParen, "')'");
        return term;
    }
    Token name = expect(TokenKind::Ident, "variable name");
    term.kind = Term::Kind::VarRef;
    term.var_name = name.lexeme;
    return term;
}

auto Parser::parse_fork_group() -> Term {
    Term term;
    term.kind = Term::Kind::Fork;
    term.line = current.line;
    term.column = current.column;
    term.branches.push_back(std::make_unique<CompExpr>(parse_comp_expr()));
    while (match(TokenKind::Pipe)) {
        skip_newlines();
        term.branches.push_back(std::make_unique<CompExpr>(parse_comp_expr()));
    }
    return term;
}

auto Parser::parse_comp_expr() -> CompExpr {
    CompExpr comp;
    while (starts_term()) comp.terms.push_back(parse_comp_term());
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
        decl.kind = VarDecl::Kind::BlockDef;
        decl.block = parse_block();
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
    while (match(TokenKind::Pipe)) stmt.machines.push_back(parse_comp_expr());
    end_statement();
    return stmt;
}

auto Parser::parse() -> Program {
    Program program;
    skip_newlines();
    while (current.kind != TokenKind::Eof) {
        if (current.kind == TokenKind::KwPlay)
            program.plays.push_back(parse_play_stmt());
        else
            program.decls.push_back(parse_var_decl());
        skip_newlines();
    }
    return program;
}
