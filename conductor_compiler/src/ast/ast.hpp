#pragma once

#include <string>

struct Program {};

class ASTPrinter {
  public:
    void operator()(const Program &program);

    auto print(const Program &program) -> std::string;
};
