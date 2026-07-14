#pragma once

#include "ast.hpp"

[[nodiscard]] auto simplify_program(const Program &program) -> Program;
