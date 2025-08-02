#include "parser.hpp"

#include "../ast/ast.hpp"
#include "tokenizer.hpp"

#include <expected>
#include <utility>
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
        while (match(TokenKind::Eol))
            advance();
        while (match(TokenKind::Colon)) {
            while (!match(TokenKind::Eol))
                advance();
            while (match(TokenKind::Eol))
                advance();
        }
        if (match(TokenKind::Eof)) break;
        auto expression = parse_expression();
        if (!expression) return expression;
        expressions.emplace_back(std::move(*expression));
    }

    return Expr::make<Block>(std::move(expressions));
}

auto Parser::parse_expression() -> ParseResult {
    auto expr = parse_additive();
    if (!expr) return std::unexpected(expr.error());

    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            return std::unexpected("Expected identifier after '>'");
        auto name = current;
        advance();
        return Expr::make<Assignment>(std::move(*expr), name);
    }

    return expr;
}

auto Parser::parse_additive() -> ParseResult {
    auto left = parse_multiplicative();
    if (!left) return left;
    while (match(TokenKind::Additive)) {
        Operation op{};
        if (current.lexeme == "+")
            op = Operation::Add;
        else if (current.lexeme == "-")
            op = Operation::Sub;
        else
            return std::unexpected("Unknown binary operation: " +
                                   current.to_string());

        advance();
        auto right = parse_multiplicative();
        if (!right) return right;
        left = Expr::make<BinaryOp>(op, std::move(*left), std::move(*right));
    }
    return left;
}

auto Parser::parse_multiplicative() -> ParseResult {
    auto left = parse_application();
    if (!left) return left;
    while (match(TokenKind::Multiplicative)) {
        Operation op{};
        if (current.lexeme == "*")
            op = Operation::Mul;
        else if (current.lexeme == "/")
            op = Operation::Div;
        else
            return std::unexpected("Unknown unary operation: " +
                                   current.to_string());

        advance();
        auto right = parse_application();
        if (!right) return right;
        left = Expr::make<BinaryOp>(op, std::move(*left), std::move(*right));
    }
    return left;
}

auto Parser::parse_application() -> ParseResult {
    ParseResult expr;
    if (match(TokenKind::Additive) && current.lexeme == "-") {
        advance();
        auto right = parse_factor();
        if (!right) return right;
        expr = Expr::make<UnaryOp>(Operation::Sub, std::move(*right));
    } else
        expr = parse_factor();
    if (!expr) return expr;

    while (true) {
        auto arg = parse_factor();
        if (!arg) break;
        expr = Expr::make<Call>(std::move(*expr), std::move(*arg));
    }
    return expr;
}

auto Parser::parse_factor() -> ParseResult {
    auto tok = current;
    if (match(TokenKind::Identifier)) {
        advance();
        return Expr::make<Variable>(tok);
    }
    if (match(TokenKind::LParen)) {
        advance();
        auto expr = parse_expression();
        if (!expr) return std::unexpected(expr.error());
        if (!match(TokenKind::RParen)) return std::unexpected("Expected ')'");
        advance();
        return expr;
    }
    if (match(TokenKind::LBrace)) {
        advance();
        return parse_lambda();
    }
    if (match(TokenKind::Number)) {
        advance();
        return Expr::make<Literal>(tok);
    }
    return std::unexpected("Unexpected token: " + current.to_string());
}

auto Parser::parse_lambda() -> ParseResult {
    auto maybe_parameter = parse_factor();
    if (!maybe_parameter) return std::unexpected("Expected a variable name");
    auto *parameter = std::get_if<Variable>(&maybe_parameter.value()->node);
    if (parameter == nullptr)
        return std::unexpected("Expected a variable name");

    if (match(TokenKind::Period)) {
        advance();
        auto block = parse_block();
        if (!match(TokenKind::RBrace))
            return std::unexpected("Expected '}': " + current.to_string());
        advance();
        return Expr::make<Lambda>(std::move(parameter->name),
                                  std::move(*block));
    }

    auto inner_lambda = parse_lambda();
    if (!inner_lambda) return std::unexpected(inner_lambda.error());
    return Expr::make<Lambda>(std::move(parameter->name),
                              std::move(*inner_lambda));
}

auto Parser::parse_block() -> ParseResult {
    std::vector<ExprPtr> expressions;
    while (true) {
        while (match(TokenKind::Eol))
            advance();
        ParseResult expression = parse_expression();
        if (!expression) break;
        expressions.emplace_back(std::move(*expression));
    }

    if (expressions.size() == 0)
        return std::unexpected("Expected an expression");

    return Expr::make<Block>(std::move(expressions));
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

    return Expr::make<Block>(std::move(expressions));
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
        return std::unexpected("Expected a a number: " + current.to_string());
    auto size = static_cast<size_t>(
        std::max(std::min(std::stoi(current.lexeme), 1024), 1) * 8);
    advance();

    if (!match(TokenKind::LBrace))
        return std::unexpected("Expected a lambda: " + current.to_string());
    advance();

    auto init_buffer_function = parse_lambda();
    if (!init_buffer_function)
        return std::unexpected("Invalid lambda: " + current.to_string());

    if (!match(TokenKind::Eol))
        return std::unexpected("Unexpected token at buffer: " +
                               current.to_string());

    return Expr::make<Buffer>(std::move(name), size,
                              std::move(*init_buffer_function));
}