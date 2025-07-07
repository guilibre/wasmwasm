#include "parser.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include <stdexcept>

Parser::Parser(Tokenizer &tokenizer)
    : tokenizer(tokenizer), current(tokenizer.next()) {}

auto Parser::parse_expr() -> ExprPtr { return parse_if(); }

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse_if() -> ExprPtr {
    if (match(TokenKind::If)) {
        advance();
        auto cond = parse_expr();
        if (!match(TokenKind::Then))
            throw std::runtime_error("Expected 'then'");
        advance();
        auto then_expr = parse_expr();
        if (!match(TokenKind::Else))
            throw std::runtime_error("Expected 'else'");
        advance();
        auto else_expr = parse_expr();
        return Expr::make<Expr::If>(std::move(cond), std::move(then_expr),
                                    std::move(else_expr));
    }
    return parse_assignment();
}

auto Parser::parse_assignment() -> ExprPtr {
    auto expr = parse_equality();
    if (match(TokenKind::Arrow)) {
        advance();
        if (current.kind != TokenKind::Identifier)
            throw std::runtime_error("Expected identifier after '->'");
        auto name = current;
        advance();
        return Expr::make<Expr::Assignment>(std::move(expr), name);
    }
    return expr;
}

auto Parser::parse_equality() -> ExprPtr {
    auto expr = parse_additive();
    while (match(TokenKind::EqEq) || match(TokenKind::BangEq)) {
        Token op = current;
        advance();
        auto right = parse_additive();
        expr = Expr::make<Expr::Binary>(op, std::move(expr), std::move(right));
    }
    return expr;
}

auto Parser::parse_additive() -> ExprPtr {
    auto expr = parse_term();
    while (match(TokenKind::Plus) || match(TokenKind::Minus)) {
        auto op = current;
        advance();
        auto right = parse_term();
        expr = Expr::make<Expr::Binary>(op, std::move(expr), std::move(right));
    }
    return expr;
}

auto Parser::parse_term() -> ExprPtr {
    auto expr = parse_factor();
    while (match(TokenKind::Star) || match(TokenKind::Slash)) {
        auto op = current;
        advance();
        auto right = parse_factor();
        expr = Expr::make<Expr::Binary>(op, std::move(expr), std::move(right));
    }
    return expr;
}

auto Parser::parse_factor() -> ExprPtr {
    if (match(TokenKind::Number)) {
        Token tok = current;
        advance();
        return Expr::make<Expr::Literal>(tok);
    }
    if (match(TokenKind::Identifier)) {
        Token tok = current;
        advance();
        return Expr::make<Expr::Variable>(tok);
    }
    if (match(TokenKind::LParen)) {
        advance();
        auto expr = parse_expr();
        if (!match(TokenKind::RParen)) throw std::runtime_error("Expected ')'");
        advance();
        return expr;
    }
    throw std::runtime_error("Unexpected token");
}