#pragma once

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

struct Duration {
    std::optional<int> numerator;
    int denominator = 1;
};

struct NoteAtom {
    bool is_rest = false;
    char letter = 'C';
    int accidental = 0;
    std::optional<int> octave;
    size_t line = 0;
    size_t column = 0;
};

struct Voice {
    NoteAtom atom;
    Duration duration;
};

struct Group;
struct SeqExpr;

struct GroupRef {
    std::unique_ptr<Group> group;
    Duration duration;
};

using Branch = std::variant<Voice, GroupRef, std::string>;

struct Term {
    std::vector<Branch> branches;
    size_t line = 0;
    size_t column = 0;
};

struct SeqExpr {
    std::vector<Term> terms;
};

struct Group {
    SeqExpr body;
};

struct MotivDef {
    std::string name;
    SeqExpr body;
};

struct PlayTarget {
    std::string name;
    size_t line = 0;
    size_t column = 0;
};

struct PlayStmt {
    std::vector<PlayTarget> targets;
};

struct Program {
    std::vector<MotivDef> motivs;
    PlayStmt play;
};

class ASTPrinter {
  public:
    void operator()(const Program &program);

    auto print(const Program &program) -> std::string;
};
