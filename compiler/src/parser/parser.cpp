#include "parser.hpp"

#include "../ast/ast.hpp"
#include "tokenizer.hpp"

#include <expected>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace {

auto precedences() -> const std::unordered_map<TokenKind, Precedence> & {
    static const std::unordered_map<TokenKind, Precedence> precedences = {
        {TokenKind::Plus, Precedence::AddSub},
        {TokenKind::Minus, Precedence::AddSub},
        {TokenKind::Star, Precedence::MulDiv},
        {TokenKind::Slash, Precedence::MulDiv},
    };
    return precedences;
}

} // namespace

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse() -> ParseResult {
    std::vector<ExprPtr> blocks;
    ParseResult block;
    while ((block = parse_block())) {
        if (block) blocks.emplace_back(std::move(*block));
    }

    if (!match(TokenKind::Eof))
        return std::unexpected("Unexpected token: " + current.to_string() +
                               " | (" + std::to_string(current.line) + "," +
                               std::to_string(current.column) + ")");

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
    auto expr = parse_infix_expr(Precedence::Lowest);
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

auto Parser::parse_infix_expr(Precedence prec) -> ParseResult {
    auto left = parse_application();
    if (!left) return std::unexpected(left.error());

    auto is_infix_op = [](TokenKind kind) {
        static const std::unordered_set<TokenKind> ops{
            TokenKind::Plus, TokenKind::Minus, TokenKind::Star,
            TokenKind::Slash};
        return ops.contains(kind);
    };

    const auto &precs = precedences();
    while (is_infix_op(current.kind)) {
        Token op = current;
        Precedence op_prec =
            precs.contains(op.kind) ? precs.at(op.kind) : Precedence::Lowest;

        if (op_prec < prec) break;

        advance();

        auto right = parse_infix_expr(
            static_cast<Precedence>(static_cast<uint8_t>(op_prec) + 1));
        if (!right) return std::unexpected(right.error());

        left =
            Expr::make<Expr::Binary>(op, std::move(*left), std::move(*right));
    }
    return left;
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
    std::vector<Expr::Variable> parameters;
    ParseResult maybe_parameter;
    while ((maybe_parameter = parse_factor())) {
        auto *parameter =
            std::get_if<Expr::Variable>(&maybe_parameter.value()->node);
        if (parameter == nullptr)
            return std::unexpected("Expected a variable name");
        parameters.emplace_back(*parameter);
    }
    if (!match(TokenKind::Period)) return std::unexpected("Expected '.'");
    advance();
    auto block = parse_block();
    if (!match(TokenKind::RBra)) return std::unexpected("Expected '}'");
    advance();
    return Expr::make<Expr::Lambda>(std::move(parameters), std::move(*block));
}