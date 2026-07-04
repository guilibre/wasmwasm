#include "ast.hpp"

#include <iostream>
#include <sstream>

namespace {

auto print_duration(const Duration &duration) -> std::string {
    if (!duration.numerator.has_value()) return "";
    std::string text = "@" + std::to_string(*duration.numerator);
    if (duration.denominator != 1)
        text += "/" + std::to_string(duration.denominator);
    return text;
}

auto print_note_atom(const NoteAtom &atom) -> std::string {
    if (atom.is_rest) return "R";
    std::string text(1, atom.letter);
    if (atom.accidental == 1) text += "#";
    if (atom.accidental == -1) text += "b";
    if (atom.octave.has_value()) text += std::to_string(*atom.octave);
    return text;
}

auto print_voice(const Voice &voice) -> std::string {
    return print_note_atom(voice.atom) + print_duration(voice.duration);
}

void print_seq_expr(const SeqExpr &expr, std::ostringstream &out);

void print_branch(const Branch &branch, std::ostringstream &out) {
    if (const auto *voice = std::get_if<Voice>(&branch)) {
        out << print_voice(*voice);
        return;
    }
    if (const auto *group_ref = std::get_if<GroupRef>(&branch)) {
        out << "(";
        print_seq_expr(group_ref->group->body, out);
        out << ")" << print_duration(group_ref->duration);
        return;
    }
    out << std::get<std::string>(branch);
}

void print_term(const Term &term, std::ostringstream &out) {
    for (size_t i = 0; i < term.branches.size(); ++i) {
        if (i > 0) out << " | ";
        print_branch(term.branches[i], out);
    }
}

void print_seq_expr(const SeqExpr &expr, std::ostringstream &out) {
    for (size_t i = 0; i < expr.terms.size(); ++i) {
        if (i > 0) out << " ";
        print_term(expr.terms[i], out);
    }
}

} // namespace

auto ASTPrinter::print(const Program &program) -> std::string {
    std::ostringstream out;

    for (const MotivDef &motiv : program.motivs) {
        out << motiv.name << " = ";
        print_seq_expr(motiv.body, out);
        out << "\n";
    }
    out << "play ";
    for (size_t i = 0; i < program.play.targets.size(); ++i) {
        if (i > 0) out << " | ";
        out << program.play.targets[i].name;
    }
    out << "\n";

    return out.str();
}

void ASTPrinter::operator()(const Program &program) {
    std::cout << print(program);
}
