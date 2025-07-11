#include <cmath>

extern "C" auto wasmwasm_sin(double x) -> double { return sin(x); }