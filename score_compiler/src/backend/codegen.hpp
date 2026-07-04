#pragma once

#include "resolve/resolver.hpp"

#include <ostream>

class CodeGenerator {
  public:
    void generate(const ResolvedProgram &program, std::ostream &out);

  private:
    void generate_preamble(std::ostream &out);
    void generate_constructor(std::ostream &out);
    void generate_run(const ResolvedProgram &program, std::ostream &out);
    void generate_track(size_t index, const ResolvedSequence &sequence,
                        std::ostream &out);
    void generate_step(const ResolvedStep &step, std::ostream &out);
};
