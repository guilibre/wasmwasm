#include "parser.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include <memory>
#include <stdexcept>
#include <unordered_map>

namespace {

auto precedences() -> const std::unordered_map<std::string_view, Precedence> & {
    static const std::unordered_map<std::string_view, Precedence> precedences =
        {
            {"+", Precedence::AddSub},
            {"-", Precedence::AddSub},
            {"*", Precedence::MulDiv},
            {"/", Precedence::MulDiv},
        };
    return precedences;
}

} // namespace

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

auto Parser::parse_expr() -> ExprPtr {
    return parse_infix_expr(Precedence::Lowest);
}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::parse_infix_expr(Precedence prec) -> ExprPtr {
    auto left = parse_application();

    while (true) {
        if (current.kind != TokenKind::Operator) break;

        Token op = current;
        Precedence op_prec = precedences().contains(op.lexeme)
                                 ? precedences().at(op.lexeme)
                                 : Precedence::Lowest;

        if (op_prec < prec) break;

        advance(); // consume operator
        auto right = parse_infix_expr(
            static_cast<Precedence>(static_cast<uint8_t>(op_prec) + 1));
        left = Expr::make<Binary>(op, std::move(left), std::move(right));
    }
    return left;
}

auto Parser::parse_assignment() -> ExprPtr {
    auto expr = parse_application();
    if (match(TokenKind::Arrow)) {
        advance();
        if (!match(TokenKind::Identifier))
            throw std::runtime_error("Expected identifier after '->'");
        auto name = current;
        advance();
        return Expr::make<Assignment>(std::move(expr), name);
    }
    return expr;
}

auto Parser::parse_application() -> ExprPtr {
    auto expr = parse_factor();

    while (true) {
        try {
            auto arg = parse_factor();
            expr = Expr::make<Call>(std::move(expr), std::move(arg));
        } catch (...) {
            break;
        }
    }

    return expr;
}

auto Parser::parse_factor() -> ExprPtr {
    auto tok = current;
    if (match(TokenKind::Number)) {
        advance();
        return Expr::make<Literal>(tok);
    }

    if (match(TokenKind::Identifier)) {
        advance();
        return Expr::make<Variable>(tok);
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