#include "parser.hpp"

#include "../ast/ast.hpp"
#include "../types/type_inference.hpp"
#include "tokenizer.hpp"

#include <expected>
#include <unordered_map>
#include <variant>
#include <vector>

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse() -> ParseResult {
    std::vector<ExprPtr> blocks;
    while (auto block = parse_block()) {
        if (!block) return std::unexpected(block.error());
        blocks.emplace_back(std::move(*block));
    }

    if (!match(TokenKind::Eof))
        return std::unexpected("Unexpected token: " + current.to_string() +
                               " | (" + std::to_string(current.line) + "," +
                               std::to_string(current.column) + ")");

    std::unordered_map<std::string_view, TypePtr> env;
    Substitution subst;

    return Expr::make<Expr::Block>(std::move(blocks));
}

auto Parser::parse_block() -> ParseResult {
    std::vector<ExprPtr> expressions;
    ParseResult expression;
    while ((expression = parse_assignment()))
        expressions.emplace_back(std::move(*expression));

    if (expressions.size() == 0) return std::unexpected("Expected a block");

    return Expr::make<Expr::Block>(std::move(expressions));
}

auto Parser::parse_assignment() -> ParseResult {
    auto expr = parse_application();
    if (!expr) return std::unexpected(expr.error());

    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            return std::unexpected("Expected identifier after '->'");
        auto name = current;
        advance();
        return Expr::make<Expr::Assignment>(std::move(*expr), name);
    }
    return expr;
}

auto Parser::parse_application() -> ParseResult {
    auto expr = parse_factor();
    if (!expr)
        return std::unexpected("Unexpected token: " + current.to_string() +
                               " | (" + std::to_string(current.line) + "," +
                               std::to_string(current.column) + ")");

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
        auto expr = parse_assignment();
        if (!expr) return std::unexpected(expr.error());
        if (!match(TokenKind::RParen)) return std::unexpected("Expected ')'");
        advance();
        return expr;
    }
    if (match(TokenKind::LBra)) {
        advance();
        auto expr = parse_lambda();
        if (!expr) return std::unexpected(expr.error());
        return expr;
    }
    return std::unexpected("Unexpected token: " + current.to_string() + " | (" +
                           std::to_string(current.line) + "," +
                           std::to_string(current.column) + ")");
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