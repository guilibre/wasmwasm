#include "ast.hpp"

namespace {

void print_expr(const Expr &expr, std::string &out) {
    if (expr.kind == Expr::Kind::Number) {
        out += std::to_string(expr.number);
        return;
    }
    out += "(";
    print_expr(*expr.lhs, out);
    switch (expr.op) {
    case BinOp::Add:
        out += " + ";
        break;
    case BinOp::Sub:
        out += " - ";
        break;
    case BinOp::Mul:
        out += " * ";
        break;
    case BinOp::Div:
        out += " / ";
        break;
    }
    print_expr(*expr.rhs, out);
    out += ")";
}

void print_comp_expr(const CompExpr &comp, std::string &out);

void print_term(const Term &term, std::string &out) {
    if (term.kind == Term::Kind::VarRef) {
        out += term.var_name;
        return;
    }
    out += "(";
    for (size_t i = 0; i < term.branches.size(); ++i) {
        if (i != 0) out += " | ";
        print_comp_expr(*term.branches[i], out);
    }
    out += ")";
}

void print_comp_expr(const CompExpr &comp, std::string &out) {
    for (size_t i = 0; i < comp.terms.size(); ++i) {
        if (i != 0) out += " ";
        print_term(comp.terms[i], out);
    }
}

} // namespace

auto ASTPrinter::print(const Program &program) -> std::string {
    std::string out;
    for (const auto &decl : program.decls) {
        out += decl.name + " = ";
        if (decl.kind == VarDecl::Kind::BlockDef) {
            out += "{ ";
            for (const auto &param : decl.block.params) {
                out += param.name + ": ";
                print_expr(*param.value, out);
                out += " ";
            }
            out += "}";
        } else {
            print_comp_expr(decl.comp, out);
        }
        out += "\n";
    }
    for (const auto &play : program.plays) {
        out += "play ";
        for (size_t i = 0; i < play.machines.size(); ++i) {
            if (i != 0) out += " | ";
            print_comp_expr(play.machines[i], out);
        }
        out += "\n";
    }
    return out;
}
