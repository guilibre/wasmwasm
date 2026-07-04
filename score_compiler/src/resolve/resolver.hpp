#pragma once

#include "ast/ast.hpp"

#include <stdexcept>
#include <string>
#include <vector>

class ResolveException : public std::runtime_error {
  public:
    ResolveException(const std::string &msg, size_t line, size_t col)
        : std::runtime_error(msg), line(line), col(col) {}

    size_t line;
    size_t col;
};

struct ResolvedNote {
    bool is_rest = false;
    int midinote = 0;
    double duration_beats = 0;
};

struct ResolvedStep {
    std::vector<ResolvedNote> notes;
    double wait_beats = 0;
};

struct ResolvedSequence {
    std::vector<ResolvedStep> steps;
};

struct ResolvedProgram {
    std::vector<ResolvedSequence> tracks;
};

auto resolve_program(const Program &program) -> ResolvedProgram;
