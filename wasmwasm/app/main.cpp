#include "code_gen.hpp"

extern "C" auto run_compiler(float sample_freq) -> int {
    return code_gen::test(sample_freq);
}

const float TEST_SAMPLE_FREQ = 1.0F / 44100.0F;

auto main(int /*argc*/, char ** /*argv*/) -> int {
    return run_compiler(TEST_SAMPLE_FREQ);
}