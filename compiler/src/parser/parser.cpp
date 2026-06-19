#include "parser.hpp"

#include "ast/ast.hpp"
#include "tokenizer.hpp"
#include <algorithm>
#include <expected>
#include <utility>
#include <variant>
#include <vector>

static constexpr int buffer_size_min = 1;
static constexpr int buffer_size_max = 1024;

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::expect_variable(const ExprPtr &expr) const
    -> std::expected<Token, ParseError> {
    const auto *var = std::get_if<Variable>(&expr->node);
    if (var == nullptr)
        return std::unexpected(ParseError{
            .msg = "Expected an identifier",
            .line = current.line,
            .col = current.column,
        });
    return var->name;
}

auto Parser::parse_code() -> ParseResult {
    std::vector<ExprPtr> expressions;

    while (true) {
        while (match(TokenKind::Eol)) advance();
        if (match(TokenKind::Eof)) break;
        auto expression = parse_expression();
        if (!expression) return expression;
        expressions.emplace_back(std::move(*expression));
    }

    return Expr::make<CodeBlock>(std::move(expressions));
}

auto Parser::parse_expression() -> ParseResult {
    auto expr = parse_additive();
    if (!expr) return std::unexpected(expr.error());

    if (match(TokenKind::Eq)) {
        auto name = expect_variable(*expr);
        if (!name)
            return std::unexpected(ParseError{
                .msg = "Expected identifier on left side of '='",
                .line = current.line,
                .col = current.column,
            });
        advance();
        auto rhs = parse_expression();
        if (!rhs) return rhs;
        return Expr::make<Bind>(*name, std::move(*rhs));
    }

    if (match(TokenKind::LeftArrow)) {
        auto target = expect_variable(*expr);
        if (!target)
            return std::unexpected(ParseError{
                .msg = "Expected identifier on left side of '<-'",
                .line = current.line,
                .col = current.column,
            });
        advance();
        auto rhs = parse_additive();
        if (!rhs) return rhs;
        return Expr::make<BufferWrite>(*target, std::move(*rhs));
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
            return std::unexpected(ParseError{
                .msg = "Unknown binary operation: " + current.to_string(),
                .line = current.line,
                .col = current.column,
            });

        advance();
        auto right = parse_multiplicative();
        if (!right) return right;
        left = Expr::make<BinaryOp>(op, std::move(*left), std::move(*right));
    }
    return left;
}

auto Parser::parse_multiplicative() -> ParseResult {
    auto left = parse_unary();
    if (!left) return left;
    while (match(TokenKind::Multiplicative)) {
        Operation op{};
        if (current.lexeme == "*")
            op = Operation::Mul;
        else if (current.lexeme == "/")
            op = Operation::Div;
        else
            return std::unexpected(ParseError{
                .msg = "Unknown binary operation: " + current.to_string(),
                .line = current.line,
                .col = current.column,
            });

        advance();
        auto right = parse_unary();
        if (!right) return right;
        left = Expr::make<BinaryOp>(op, std::move(*left), std::move(*right));
    }
    return left;
}

auto Parser::parse_unary() -> ParseResult {
    if (match(TokenKind::Additive) && current.lexeme == "-") {
        advance();
        auto operand = parse_application();
        if (!operand) return operand;
        return Expr::make<UnaryOp>(Operation::Sub, std::move(*operand));
    }
    return parse_application();
}

auto Parser::parse_application() -> ParseResult {
    auto expr = parse_factor();
    if (!expr) return expr;

    while (true) {
        auto arg = parse_factor();
        if (!arg) break;
        expr = Expr::make<Call>(std::move(*expr), std::move(*arg));
    }
    return expr;
}

auto Parser::parse_factor() -> ParseResult {
    const auto tok = current;

    if (match(TokenKind::At)) {
        advance();
        std::optional<ExprPtr> delay;
        if (match(TokenKind::LBracket)) {
            advance();
            auto d = parse_expression();
            if (!d) return std::unexpected(d.error());
            delay = std::move(*d);
            if (!match(TokenKind::RBracket))
                return std::unexpected(ParseError{
                    .msg = "Expected ']' after delay expression",
                    .line = current.line,
                    .col = current.column,
                });
            advance();
        }
        if (!match(TokenKind::Identifier))
            return std::unexpected(ParseError{
                .msg = "Expected identifier after '@'",
                .line = current.line,
                .col = current.column,
            });
        auto name = current;
        advance();
        auto e = Expr::make<BufferRead>(name, std::move(delay));
        e->pos = {.line = name.line, .col = name.column};
        return e;
    }

    if (match(TokenKind::Delay)) { return parse_buffer_ctor(); }

    if (match(TokenKind::Identifier)) {
        advance();
        auto e = Expr::make<Variable>(tok);
        e->pos = {.line = tok.line, .col = tok.column};
        return e;
    }

    if (match(TokenKind::LParen)) {
        advance();
        auto expr = parse_expression();
        if (!expr) return std::unexpected(expr.error());
        if (!match(TokenKind::RParen))
            return std::unexpected(ParseError{
                .msg = "Expected ')'",
                .line = current.line,
                .col = current.column,
            });
        advance();
        return expr;
    }

    if (match(TokenKind::LBrace)) {
        advance();
        return parse_lambda();
    }

    if (match(TokenKind::Number)) {
        advance();
        auto e = Expr::make<Literal>(tok);
        e->pos = {.line = tok.line, .col = tok.column};
        return e;
    }

    return std::unexpected(ParseError{
        .msg = "Unexpected token: " + current.to_string(),
        .line = current.line,
        .col = current.column,
    });
}

auto Parser::parse_lambda() -> ParseResult {
    const auto maybe_parameter = parse_factor();
    if (!maybe_parameter)
        return std::unexpected(ParseError{
            .msg = "Expected a variable name",
            .line = current.line,
            .col = current.column,
        });
    auto *parameter = std::get_if<Variable>(&maybe_parameter.value()->node);
    if (parameter == nullptr)
        return std::unexpected(ParseError{
            .msg = "Expected a variable name",
            .line = current.line,
            .col = current.column,
        });

    if (match(TokenKind::Period)) {
        advance();
        auto block = parse_block();
        if (!match(TokenKind::RBrace))
            return std::unexpected(ParseError{
                .msg = "Expected '}': " + current.to_string(),
                .line = current.line,
                .col = current.column,
            });
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
        while (match(TokenKind::Eol)) advance();
        ParseResult expression = parse_expression();
        if (!expression) break;
        expressions.emplace_back(std::move(*expression));
    }

    if (expressions.empty())
        return std::unexpected(ParseError{
            .msg = "Expected an expression",
            .line = current.line,
            .col = current.column,
        });

    return Expr::make<CodeBlock>(std::move(expressions));
}

auto Parser::parse_buffer_ctor() -> ParseResult {
    advance();

    if (!match(TokenKind::Number))
        return std::unexpected(ParseError{
            .msg = "Expected a size after 'delay': " + current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    const auto size = static_cast<size_t>(std::clamp(
        std::stoi(current.lexeme), buffer_size_min, buffer_size_max));
    advance();

    if (!match(TokenKind::LBrace))
        return std::unexpected(ParseError{
            .msg = "Expected a lambda after size in 'delay': " +
                   current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    advance();

    auto init_fn = parse_lambda();
    if (!init_fn)
        return std::unexpected(ParseError{
            .msg = "Invalid lambda in 'delay': " + current.to_string(),
            .line = current.line,
            .col = current.column,
        });

    return Expr::make<BufferCtor>(size, std::move(*init_fn));
}
