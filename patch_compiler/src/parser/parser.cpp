#include "parser.hpp"

#include "ast/ast.hpp"
#include "tokenizer.hpp"
#include <algorithm>
#include <expected>
#include <utility>
#include <variant>
#include <vector>

static constexpr size_t delay_size_min = 1;
static constexpr size_t delay_size_max = 64UL * 1024;

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
    if (match(TokenKind::Static)) {
        advance();
        if (!match(TokenKind::Identifier))
            return std::unexpected(ParseError{
                .msg = "Expected identifier after 'static'",
                .line = current.line,
                .col = current.column,
            });
        auto name = current;
        advance();
        if (!match(TokenKind::Eq))
            return std::unexpected(ParseError{
                .msg = "Expected '=' after static variable name",
                .line = current.line,
                .col = current.column,
            });
        advance();
        auto init = parse_expression();
        if (!init) return init;
        auto e = Expr::make<StaticBind>(name, std::move(*init));
        e->pos = {.line = name.line, .col = name.column};
        return e;
    }

    if (match(TokenKind::Param)) {
        advance();
        if (!match(TokenKind::Identifier))
            return std::unexpected(ParseError{
                .msg = "Expected identifier after 'param'",
                .line = current.line,
                .col = current.column,
            });
        auto name = current;
        advance();
        if (!match(TokenKind::Eq))
            return std::unexpected(ParseError{
                .msg = "Expected '=' after param name",
                .line = current.line,
                .col = current.column,
            });
        advance();
        auto default_val = parse_expression();
        if (!default_val) return default_val;
        auto e = Expr::make<ParamBind>(name, std::move(*default_val));
        e->pos = {.line = name.line, .col = name.column};
        return e;
    }

    if (match(TokenKind::Out)) {
        const auto out_tok = current;
        advance();
        if (!match(TokenKind::LBracket))
            return std::unexpected(ParseError{
                .msg = "Expected '[' after OUT",
                .line = current.line,
                .col = current.column,
            });
        advance();
        if (!match(TokenKind::Number))
            return std::unexpected(ParseError{
                .msg = "Expected integer index in OUT[n]",
                .line = current.line,
                .col = current.column,
            });
        const size_t index = std::stoul(current.lexeme);
        advance();
        if (!match(TokenKind::RBracket))
            return std::unexpected(ParseError{
                .msg = "Expected ']' after OUT index",
                .line = current.line,
                .col = current.column,
            });
        advance();
        if (!match(TokenKind::LeftArrow))
            return std::unexpected(ParseError{
                .msg = "Expected '<-' after OUT[n]",
                .line = current.line,
                .col = current.column,
            });
        advance();
        auto rhs = parse_logical_or();
        if (!rhs) return rhs;
        auto e = Expr::make<OutputWrite>(index, std::move(*rhs));
        e->pos = {.line = out_tok.line, .col = out_tok.column};
        return e;
    }

    const auto expr_start = current;
    auto expr = parse_logical_or();
    if (!expr) return std::unexpected(expr.error());

    if (match(TokenKind::Question)) {
        advance();
        auto then_b = parse_expression();
        if (!then_b) return then_b;
        std::optional<ExprPtr> else_b;
        if (match(TokenKind::Colon)) {
            advance();
            auto eb = parse_expression();
            if (!eb) return eb;
            else_b = std::move(*eb);
        }
        auto e = Expr::make<Conditional>(std::move(*expr), std::move(*then_b),
                                         std::move(else_b));
        e->pos = {.line = expr_start.line, .col = expr_start.column};
        return e;
    }

    if (match(TokenKind::Eq)) {
        if (auto *dr = std::get_if<DelayRead>(&(*expr)->node)) {
            auto target = dr->name;
            std::optional<ExprPtr> delay;
            if (dr->delay) { delay = std::move(*dr->delay); }
            advance();
            auto rhs = parse_expression();
            if (!rhs) return rhs;
            auto e = Expr::make<DelayWriteQuiet>(target, std::move(delay),
                                                 std::move(*rhs));
            e->pos = {.line = target.line, .col = target.column};
            return e;
        }
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
        auto e = Expr::make<Bind>(*name, std::move(*rhs), std::string{});
        e->pos = {.line = name->line, .col = name->column};
        return e;
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
        auto rhs = parse_logical_or();
        if (!rhs) return rhs;
        auto e = Expr::make<DelayWrite>(*target, std::move(*rhs));
        e->pos = {.line = target->line, .col = target->column};
        return e;
    }

    return expr;
}

auto Parser::parse_logical_or() -> ParseResult {
    auto left = parse_logical_and();
    if (!left) return left;
    while (match(TokenKind::Ampersand)) {
        advance();
        auto right = parse_logical_and();
        if (!right) return right;
        left = Expr::make<BinaryOp>(Operation::Or, std::move(*left),
                                    std::move(*right));
    }
    return left;
}

auto Parser::parse_logical_and() -> ParseResult {
    auto left = parse_comparison();
    if (!left) return left;
    while (match(TokenKind::Ampersand)) {
        advance();
        auto right = parse_comparison();
        if (!right) return right;
        left = Expr::make<BinaryOp>(Operation::And, std::move(*left),
                                    std::move(*right));
    }
    return left;
}

auto Parser::parse_comparison() -> ParseResult {
    auto left = parse_additive();
    if (!left) return left;
    if (match(TokenKind::Comparison)) {
        const auto op = [&] -> Operation {
            if (current.lexeme == "<") return Operation::Lt;
            if (current.lexeme == ">") return Operation::Gt;
            if (current.lexeme == "<=") return Operation::Le;
            if (current.lexeme == ">=") return Operation::Ge;
            if (current.lexeme == "==") return Operation::Eq;
            return Operation::Ne;
        }();
        advance();
        auto right = parse_additive();
        if (!right) return right;
        return Expr::make<BinaryOp>(op, std::move(*left), std::move(*right));
    }
    return left;
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
    auto left = parse_power();
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

auto Parser::parse_power() -> ParseResult {
    auto left = parse_unary();
    if (!left) return left;
    if (match(TokenKind::Caret)) {
        advance();
        auto right = parse_power();
        if (!right) return right;
        left = Expr::make<BinaryOp>(Operation::Pow, std::move(*left),
                                    std::move(*right));
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
    if (match(TokenKind::Bang)) {
        advance();
        auto operand = parse_application();
        if (!operand) return operand;
        return Expr::make<UnaryOp>(Operation::Not, std::move(*operand));
    }
    return parse_application();
}

auto Parser::parse_application() -> ParseResult {
    const auto call_pos = current;
    auto expr = parse_factor();
    if (!expr) return expr;

    while (true) {
        auto arg = parse_factor();
        if (!arg) break;
        expr = Expr::make<Call>(std::move(*expr), std::move(*arg));
        (*expr)->pos = {.line = call_pos.line, .col = call_pos.column};
    }
    return expr;
}

auto Parser::parse_factor() -> ParseResult {
    const auto tok = current;

    if (match(TokenKind::In)) {
        advance();
        if (!match(TokenKind::LBracket))
            return std::unexpected(ParseError{
                .msg = "Expected '[' after IN",
                .line = current.line,
                .col = current.column,
            });
        advance();
        if (!match(TokenKind::Number))
            return std::unexpected(ParseError{
                .msg = "Expected integer index in IN[n]",
                .line = current.line,
                .col = current.column,
            });
        const size_t index = std::stoul(current.lexeme);
        advance();
        if (!match(TokenKind::RBracket))
            return std::unexpected(ParseError{
                .msg = "Expected ']' after IN index",
                .line = current.line,
                .col = current.column,
            });
        advance();
        return Expr::make<InputRead>(index);
    }

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
        auto e = Expr::make<DelayRead>(name, std::move(delay));
        e->pos = {.line = name.line, .col = name.column};
        return e;
    }

    if (match(TokenKind::Delay)) { return parse_delay_ctor(); }

    if (match(TokenKind::Array)) { return parse_array_ctor(); }

    if (match(TokenKind::LBracket)) {
        auto lit = parse_array_literal();
        if (!lit) return lit;
        return parse_postfix_index(std::move(*lit), tok);
    }

    if (match(TokenKind::Identifier)) {
        advance();
        const bool bracket_is_adjacent =
            match(TokenKind::LBracket) && current.line == tok.line &&
            current.column == tok.column + tok.lexeme.size();
        if (bracket_is_adjacent) {
            advance();
            auto idx = parse_expression();
            if (!idx) return std::unexpected(idx.error());
            if (!match(TokenKind::RBracket))
                return std::unexpected(ParseError{
                    .msg = "Expected ']' after array index",
                    .line = current.line,
                    .col = current.column,
                });
            advance();
            auto e =
                Expr::make<ArrayIndex>(tok, std::move(*idx), std::string{});
            e->pos = {.line = tok.line, .col = tok.column};
            return e;
        }
        auto e = Expr::make<Variable>(tok, std::string{});
        e->pos = {.line = tok.line, .col = tok.column};
        return e;
    }

    if (match(TokenKind::LParen)) {
        advance();
        auto expr = parse_block();
        if (!expr) return std::unexpected(expr.error());
        if (!match(TokenKind::RParen))
            return std::unexpected(ParseError{
                .msg = "Expected ')'",
                .line = current.line,
                .col = current.column,
            });
        const auto close_tok = current;
        advance();
        return parse_postfix_index(std::move(*expr), close_tok);
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
        return Expr::make<Lambda>(std::nullopt, std::move(*block));
    }

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

auto Parser::parse_postfix_index(ExprPtr base, const Token &pos_tok)
    -> ParseResult {
    if (!match(TokenKind::LBracket)) return base;
    advance();
    auto idx = parse_expression();
    if (!idx) return std::unexpected(idx.error());
    if (!match(TokenKind::RBracket))
        return std::unexpected(ParseError{
            .msg = "Expected ']' after array index",
            .line = current.line,
            .col = current.column,
        });
    advance();
    auto e = Expr::make<ExprIndex>(std::move(base), std::move(*idx));
    e->pos = {.line = pos_tok.line, .col = pos_tok.column};
    return e;
}

auto Parser::parse_array_literal() -> ParseResult {
    const auto start_tok = current;
    advance();
    std::vector<ExprPtr> elements;
    while (match(TokenKind::Eol)) advance();
    while (!match(TokenKind::RBracket)) {
        if (match(TokenKind::Eof))
            return std::unexpected(ParseError{
                .msg = "Unterminated array literal",
                .line = current.line,
                .col = current.column,
            });
        auto elem = parse_logical_or();
        if (!elem) return elem;
        elements.emplace_back(std::move(*elem));
        if (match(TokenKind::Comma)) advance();
        while (match(TokenKind::Eol)) advance();
    }
    if (elements.empty())
        return std::unexpected(ParseError{
            .msg = "Array literal must have at least one element",
            .line = current.line,
            .col = current.column,
        });
    advance();
    auto e = Expr::make<ArrayLiteral>(std::move(elements));
    e->pos = {.line = start_tok.line, .col = start_tok.column};
    return e;
}

auto Parser::parse_array_ctor() -> ParseResult {
    advance();
    if (!match(TokenKind::Number))
        return std::unexpected(ParseError{
            .msg = "Expected a size after 'array': " + current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    const auto size = std::stoul(current.lexeme);
    if (size < 1)
        return std::unexpected(ParseError{
            .msg = "Array size must be at least 1",
            .line = current.line,
            .col = current.column,
        });
    advance();
    if (!match(TokenKind::LBrace))
        return std::unexpected(ParseError{
            .msg = "Expected a lambda after size in 'array': " +
                   current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    advance();
    auto init_fn = parse_lambda();
    if (!init_fn)
        return std::unexpected(ParseError{
            .msg = "Invalid lambda in 'array': " + current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    return Expr::make<ArrayCtor>(size, std::move(*init_fn));
}

auto Parser::parse_delay_ctor() -> ParseResult {
    advance();

    if (!match(TokenKind::Number))
        return std::unexpected(ParseError{
            .msg = "Expected a size after 'delay': " + current.to_string(),
            .line = current.line,
            .col = current.column,
        });
    const auto size = static_cast<size_t>(
        std::clamp(std::stoul(current.lexeme), delay_size_min, delay_size_max));
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

    return Expr::make<DelayCtor>(size, std::move(*init_fn));
}
