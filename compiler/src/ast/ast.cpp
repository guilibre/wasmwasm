#include "ast.hpp"

#include <iostream>
#include <sstream>
#include <string>
#include <variant>

auto ASTPrinter::print(const ExprPtr &expr, size_t indent) // NOLINT
    -> std::string {
    return std::visit([&](auto &node) { return dispatch(expr, node, indent); },
                      expr->node);
}

void ASTPrinter::operator()(const ExprPtr &expr) { std::cout << print(expr); }

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Assignment &asg,
                          size_t indent) -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Assignment(" << asg.name.lexeme << ")\n";
    out << print(asg.value, indent + 2);
    return attach_type(out.str(), expr, indent);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Block &block,
                          size_t indent) -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Block\n";
    for (auto &e : block.expressions) {
        out << print(e, indent + 2);
    }
    return attach_type(out.str(), expr, indent);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Call &call, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Call\n";
    out << print(call.callee, indent + 2);
    out << print(call.argument, indent + 2);
    return attach_type(out.str(), expr, indent);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Lambda &lam, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Lambda(" << lam.parameter.lexeme << ")\n";
    out << print(lam.body, indent + 2);
    return attach_type(out.str(), expr, indent);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Literal &lit,
                          size_t indent) -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Literal(" << lit.value.lexeme << ")";
    return attach_type(out.str(), expr, indent, true);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Variable &var,
                          size_t indent) -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Variable(" << var.name.lexeme << ")";
    return attach_type(out.str(), expr, indent, true);
}

auto ASTPrinter::dispatch(const ExprPtr &expr, Expr::Buffer &buf, size_t indent)
    -> std::string {
    std::ostringstream out;
    out << indent_str(indent) << "Buffer(" << buf.name << ", " << buf.size
        << ")\n"
        << print(buf.init_buffer_function, indent + 2);
    return attach_type(out.str(), expr, indent);
}

auto ASTPrinter::tokenkind_to_string(TokenKind kind) -> std::string {
    switch (kind) {
    case TokenKind::Arrow:
        return ">";
    case TokenKind::LParen:
        return "(";
    case TokenKind::RParen:
        return ")";
    case TokenKind::LBra:
        return "{";
    case TokenKind::RBra:
        return "}";
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

auto ASTPrinter::attach_type(const std::string &str, const ExprPtr &expr,
                             size_t indent, bool inline_type) -> std::string {
    if (!expr->type) return str + (inline_type ? "\n" : "");
    std::ostringstream out;
    if (inline_type) {
        out << " : " << type_to_string(expr->type) << "\n";
    } else {
        out << indent_str(indent) << "[type: " << type_to_string(expr->type)
            << "]\n";
    }
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