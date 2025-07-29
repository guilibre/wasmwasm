#include "ast.hpp"

#include <iostream>
#include <sstream>
#include <string>
#include <variant>

namespace {

auto op_to_string(const Operation &op) -> std::string {
    switch (op) {
    case Add:
        return "+";
    case Sub:
        return "-";
    case Mul:
        return "*";
    case Div:
        return "/";
    }
    return "?";
}

} // namespace

auto ASTPrinter::print(const ExprPtr &expr, size_t indent) // NOLINT
    -> std::string {
    return std::visit([&](auto &node) { return dispatch(expr, node, indent); },
                      expr->node);
}

void ASTPrinter::operator()(const ExprPtr &expr) { std::cout << print(expr); }

auto ASTPrinter::dispatch(const ExprPtr &expr, Assignment &asg, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Assignment(" + asg.name.lexeme + ")", expr);
    out << print(asg.value, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, BinaryOp &op, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Binary(" + op_to_string(op.op) + ")", expr);
    out << print(op.left, indent + 2);
    out << print(op.right, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Block &block, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << attach_type("Block", expr);
    for (auto &e : block.expressions)
        out << print(e, indent + 2);

    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Buffer &buf, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type(
        "Buffer(" + buf.name + ", " + std::to_string(buf.size) + ")", expr);
    out << indent_str(indent) << print(buf.init_buffer_function, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Call &call, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << attach_type("Call", expr);
    out << print(call.callee, indent + 2);
    out << print(call.argument, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Lambda &lam, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Lambda(" + lam.parameter.lexeme + ")", expr);
    out << print(lam.body, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Literal &lit, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Literal(" + lit.value.lexeme + ")", expr);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, UnaryOp &op, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Unary(" + op_to_string(op.op) + ")", expr);
    out << print(op.expr, indent + 2);
    return out.str();
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Variable &var, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent);
    out << attach_type("Variable(" + var.name.lexeme + ")", expr);
    return out.str();
}

auto ASTPrinter::tokenkind_to_string(TokenKind kind) -> std::string {
    switch (kind) {
    case TokenKind::Arrow:
        return ">";
    case TokenKind::LParen:
        return "(";
    case TokenKind::RParen:
        return ")";
    case TokenKind::LBrace:
        return "{";
    case TokenKind::RBrace:
        return "}";
    case TokenKind::Period:
        return ".";
    case TokenKind::Comma:
        return ",";
    case TokenKind::Eol:
        return "\\n";
    case TokenKind::Identifier:
        return "identifier";
    case TokenKind::Number:
        return "number";
    case TokenKind::Eof:
        return "<eof>";
    default:
        return "<unknown>";
    }
}

auto ASTPrinter::indent_str(size_t indent) const -> std::string { // NOLINT
    return std::string(indent, ' ');                              // NOLINT
}

auto ASTPrinter::attach_type(const std::string &str, const ExprPtr &expr)
    -> std::string {
    std::ostringstream out;
    out << ": " << type_to_string(expr->type) << '\n';
    return str + out.str();
}

auto ASTPrinter::type_to_string(const TypePtr &type) -> std::string {
    if (auto *base = std::get_if<TypeBase>(&type->node)) {
        switch (base->kind) {
        case BaseTypeKind::Bool:
            return "bool";
        case BaseTypeKind::Float:
            return "float";
        case BaseTypeKind::Int:
            return "int";
        case BaseTypeKind::Void:
            return "void";
        default:
            return "<?>";
        }
    }

    if (auto *fun = std::get_if<TypeFun>(&type->node))
        return "(" + type_to_string(fun->param) + " -> " +
               type_to_string(fun->result) + ")";

    if (auto *var = std::get_if<TypeVar>(&type->node))
        return "t" + std::to_string(var->id);

    return "<?>";
}