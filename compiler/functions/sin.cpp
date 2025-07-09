#include <cmath>

extern "C" auto wasmwasm_sin(float x) -> float { return sinf(x); }