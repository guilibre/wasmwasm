#include "ast.hpp"

namespace {

void print_expr(const Expr &expr, std::string &out) {
    if (expr.kind == Expr::Kind::Number) {
        out += std::to_string(expr.number);
        return;
    }
    if (expr.kind == Expr::Kind::String) {
        out += "\"" + expr.string_value + "\"";
        return;
    }
    if (expr.kind == Expr::Kind::Null) {
        out += "null";
        return;
    }
    if (expr.kind == Expr::Kind::Skip) {
        out += "skip";
        return;
    }
    if (expr.kind == Expr::Kind::Ident) {
        out += expr.ident_name;
        return;
    }
    if (expr.kind == Expr::Kind::Array) {
        out += "[";
        for (size_t i = 0; i < expr.elements.size(); ++i) {
            if (i != 0) out += ", ";
            print_expr(*expr.elements[i], out);
        }
        out += "]";
        return;
    }
    if (expr.kind == Expr::Kind::Ternary) {
        out += "(";
        print_expr(*expr.ternary_cond, out);
        out += " ? ";
        print_expr(*expr.ternary_then, out);
        out += " : ";
        print_expr(*expr.ternary_else, out);
        out += ")";
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
    case BinOp::Mod:
        out += " % ";
        break;
    case BinOp::Pow:
        out += " ^ ";
        break;
    case BinOp::Eq:
        out += " == ";
        break;
    case BinOp::NotEq:
        out += " != ";
        break;
    case BinOp::Lt:
        out += " < ";
        break;
    case BinOp::Gt:
        out += " > ";
        break;
    case BinOp::LtEq:
        out += " <= ";
        break;
    case BinOp::GtEq:
        out += " >= ";
        break;
    case BinOp::And:
        out += " && ";
        break;
    case BinOp::Or:
        out += " || ";
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
    if (term.kind == Term::Kind::BlockLit) {
        out += "{block}";
        return;
    }
    if (term.kind == Term::Kind::AtomicJoin) {
        print_comp_expr(*term.lhs_expr, out);
        out +=
            "@" + (term.rhs_is_block ? std::string("{block}") : term.rhs_name);
        return;
    }
    if (term.kind == Term::Kind::Choose) {
        out += "choose ";
        print_expr(*term.pipe_expr, out);
        out += " ";
        print_comp_expr(*term.branches[0], out);
        out += " ";
        print_comp_expr(*term.branches[1], out);
        return;
    }
    if (term.kind == Term::Kind::Emit) {
        out += "emit \"" + term.rhs_name + "\" {block}";
        return;
    }
    if (term.kind == Term::Kind::Pipe) {
        print_comp_expr(*term.lhs_expr, out);
        switch (term.pipe_op) {
        case Term::PipeOp::Reverse:
            out += " |> reverse";
            break;
        case Term::PipeOp::Repeat:
            out += " |> repeat ";
            print_expr(*term.pipe_expr, out);
            break;
        case Term::PipeOp::Listen:
            out += " |> listen \"" + term.rhs_name + "\"";
            break;
        case Term::PipeOp::Skip:
            out += " |> skip \"" + term.rhs_name + "\"";
            break;
        }
        return;
    }
    out += "(";
    for (size_t i = 0; i < term.branches.size(); ++i) {
        if (i != 0) out += " & ";
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
        } else if (decl.kind == VarDecl::Kind::ScaleDef) {
            out += "[";
            for (size_t i = 0; i < decl.scale.size(); ++i) {
                if (i != 0) out += ", ";
                print_expr(*decl.scale[i], out);
            }
            out += "]";
        } else {
            print_comp_expr(decl.comp, out);
        }
        out += "\n";
    }
    for (const auto &play : program.plays) {
        out += "play ";
        for (size_t i = 0; i < play.machines.size(); ++i) {
            if (i != 0) out += " & ";
            print_comp_expr(play.machines[i], out);
        }
        out += "\n";
    }
    return out;
}
