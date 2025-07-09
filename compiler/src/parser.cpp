#include "parser.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include <cstdint>
#include <stdexcept>
#include <string>
#include <unordered_map>
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

auto Parser::parse_assignments() -> std::vector<ExprPtr> {
    std::vector<ExprPtr> assignments;
    auto assignment = parse_assignment();
    while (assignment) {
        assignments.push_back(std::move(assignment));
        assignment = parse_assignment();
    }
    if (!match(TokenKind::Eof))
        throw std::runtime_error(
            "Unexpected token: " +
            std::to_string(static_cast<uint8_t>(current.kind)) + " | (" +
            std::to_string(current.line) + "," +
            std::to_string(current.column) + ")");

    advance();
    return assignments;
}

auto Parser::parse_assignment() -> ExprPtr {
    auto expr = parse_infix_expr(Precedence::Lowest);
    if (!expr) return nullptr;

    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            throw std::runtime_error("Expected identifier after '->'");
        auto name = current;
        advance();
        return Expr::make<Expr::Assignment>(std::move(expr), name);
    }
    return expr;
}

auto Parser::parse_infix_expr(Precedence prec) -> ExprPtr {
    auto left = parse_application();
    if (!left) return nullptr;

    while (true) {
        if (current.kind != TokenKind::Plus &&
            current.kind != TokenKind::Minus &&
            current.kind != TokenKind::Star && current.kind != TokenKind::Slash)
            break;

        Token op = current;
        Precedence op_prec = precedences().contains(op.kind)
                                 ? precedences().at(op.kind)
                                 : Precedence::Lowest;

        if (op_prec < prec) break;

        advance(); // consume operator
        auto right = parse_infix_expr(
            static_cast<Precedence>(static_cast<uint8_t>(op_prec) + 1));
        left = Expr::make<Expr::Binary>(op, std::move(left), std::move(right));
    }
    return left;
}

auto Parser::parse_application() -> ExprPtr {
    auto expr = parse_factor();
    if (!expr) return nullptr;

    while (true) {
        auto arg = parse_factor();
        if (!arg) break;
        expr = Expr::make<Expr::Call>(std::move(expr), std::move(arg));
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
        auto expr = parse_assignment();
        if (!match(TokenKind::RParen)) throw std::runtime_error("Expected ')'");
        advance();
        return expr;
    }

    return nullptr;
}