#include "parser.hpp"

#include "../ast/ast.hpp"
#include "tokenizer.hpp"

#include <expected>
#include <variant>
#include <vector>

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse_code() -> ParseResult {
    std::vector<ExprPtr> expressions;

    while (true) {
        while (match(TokenKind::Colon)) {
            while (!match(TokenKind::Eol))
                advance();
            advance();
        }
        while (match(TokenKind::Eol))
            advance();
        if (match(TokenKind::Eof)) break;
        auto expression = parse_expression();
        if (!expression) return expression;
        expressions.emplace_back(std::move(*expression));
    }

    return Expr::make<Expr::Block>(std::move(expressions));
}

auto Parser::parse_expression() -> ParseResult {
    auto expr = parse_application();
    if (!expr) return std::unexpected(expr.error());

    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            return std::unexpected("Expected identifier after '>'");
        auto name = current;
        advance();
        return Expr::make<Expr::Assignment>(std::move(*expr), name);
    }

    return expr;
}

auto Parser::parse_application() -> ParseResult {
    auto expr = parse_factor();
    if (!expr) return expr;

    while (true) {
        auto arg = parse_factor();
        if (!arg) break;
        expr = Expr::make<Expr::Call>(std::move(*expr), std::move(*arg));
    }
    return expr;
}

auto Parser::parse_factor() -> ParseResult {
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
        auto expr = parse_expression();
        if (!expr) return std::unexpected(expr.error());
        if (!match(TokenKind::RParen)) return std::unexpected("Expected ')'");
        advance();
        return expr;
    }
    if (match(TokenKind::LBra)) {
        advance();
        auto expr = parse_lambda();
        if (!expr) return expr;
        return expr;
    }
    if (match(TokenKind::Eol)) {
        advance();
        return parse_factor();
    }
    return std::unexpected("Unexpected token: " + current.to_string());
}

auto Parser::parse_lambda() -> ParseResult {
    ParseResult maybe_parameter = parse_factor();
    if (!maybe_parameter) return std::unexpected("Expected a variable name");
    auto *parameter =
        std::get_if<Expr::Variable>(&maybe_parameter.value()->node);
    if (parameter == nullptr)
        return std::unexpected("Expected a variable name");

    if (match(TokenKind::Period)) {
        advance();
        auto block = parse_block();
        if (!match(TokenKind::RBra)) return std::unexpected("Expected '}'");
        advance();
        return Expr::make<Expr::Lambda>(std::move(parameter->name),
                                        std::move(*block));
    }

    auto inner_lambda = parse_lambda();
    if (!inner_lambda) return std::unexpected(inner_lambda.error());
    return Expr::make<Expr::Lambda>(std::move(parameter->name),
                                    std::move(*inner_lambda));
}

auto Parser::parse_block() -> ParseResult {
    std::vector<ExprPtr> expressions;
    ParseResult expression;
    while ((expression = parse_expression()))
        expressions.emplace_back(std::move(*expression));

    if (expressions.size() == 0)
        return std::unexpected("Expected an expression");

    return Expr::make<Expr::Block>(std::move(expressions));
}

auto Parser::parse_initialization() -> ParseResult {
    std::vector<ExprPtr> expressions;

    while (true) {
        while (match(TokenKind::Colon)) {
            advance();
            auto expression = parse_buffer();
            if (!expression) return expression;
            expressions.emplace_back(std::move(*expression));
            advance();
        }
        while (!match(TokenKind::Eol) && !match(TokenKind::Eof))
            advance();
        while (match(TokenKind::Eol))
            advance();
        if (match(TokenKind::Eof)) break;
    }

    return Expr::make<Expr::Block>(std::move(expressions));
}

auto Parser::parse_buffer() -> ParseResult {
    if (!match(TokenKind::Identifier) || current.lexeme != "buffer")
        return std::unexpected("Expected 'buffer': " + current.to_string());
    advance();

    if (!match(TokenKind::Identifier))
        return std::unexpected("Expected an identifier: " +
                               current.to_string());
    auto name = current.lexeme;
    advance();

    if (!match(TokenKind::Number))
        return std::unexpected("Expected a number: " + current.to_string());
    auto size = static_cast<size_t>(std::max(
                    std::min(std::stod(current.lexeme), 1024.0), 1.0)) *
                8;
    advance();

    if (!match(TokenKind::LBra))
        return std::unexpected("Expected a lambda: " + current.to_string());
    advance();

    auto init_buffer_function = parse_lambda();
    if (!init_buffer_function)
        return std::unexpected("Invalid lambda: " + current.to_string());

    if (!match(TokenKind::Eol))
        return std::unexpected("Unexpected token at buffer: " +
                               current.to_string());

    return Expr::make<Expr::Buffer>(std::move(name), size,
                                    std::move(*init_buffer_function));
}