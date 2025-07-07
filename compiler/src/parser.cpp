#include "parser.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include <stdexcept>

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

auto Parser::parse_expr() -> ExprPtr { return parse_assignment(); }

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse_assignment() -> ExprPtr {
    auto expr = parse_additive();
    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            throw std::runtime_error("Expected identifier after '->'");
        auto name = current;
        advance();
        return Expr::make<Expr::Assignment>(std::move(expr), name);
    }
    throw std::runtime_error("Expected an assignment");
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
    auto tok = current;
    if (match(TokenKind::Number)) {
        advance();
        return Expr::make<Expr::Literal>(tok);
    }

    if (match(TokenKind::Identifier)) {
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