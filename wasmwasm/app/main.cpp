#include "tokenizer.hpp"
#include <exception>
#include <iostream>
#include <string>

extern "C" {
auto main(int argc, char **argv) -> int { // NOLINT
    try {
        if (argc < 2) {
            std::cout << "Source code not provided." << '\n';
            return 1;
        }

        tokenizer::test(argv[1]); // NOLINT
    } catch (std::exception &ex) {
        std::cout << "error in compiler: " << ex.what() << '\n';
        return 1;
    }
    return 0;
}
}