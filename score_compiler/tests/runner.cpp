#include "compile.hpp"
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace {

struct StdinFixture {
    std::string score_source;
    std::string expect_text;
};

auto read_fixture_from_stdin() -> StdinFixture {
    std::string len_line;
    if (!std::getline(std::cin, len_line))
        throw std::runtime_error("stdin: missing score-source length header");

    size_t score_len = 0;
    try {
        score_len = std::stoul(len_line);
    } catch (const std::exception &) {
        throw std::runtime_error("stdin: invalid score-source length header: " +
                                 len_line);
    }

    std::string score_source(score_len, '\0');
    std::cin.read(score_source.data(), static_cast<std::streamsize>(score_len));
    if (static_cast<size_t>(std::cin.gcount()) != score_len)
        throw std::runtime_error("stdin: truncated score source");

    std::ostringstream rest;
    rest << std::cin.rdbuf();
    return {.score_source = std::move(score_source), .expect_text = rest.str()};
}

struct Assertion {
    bool expect_present;
    std::string needle;
};

struct Expectations {
    std::vector<Assertion> assertions;
    bool expect_error = false;
};

auto parse_expectations(const std::string &text) -> Expectations {
    Expectations expect;
    std::istringstream ss(text);
    std::string line;
    size_t line_no = 0;
    while (std::getline(ss, line)) {
        ++line_no;
        if (!line.empty() && line.back() == '\r') line.pop_back();

        const auto first_non_space = line.find_first_not_of(" \t");
        if (first_non_space == std::string::npos) continue;
        if (line[first_non_space] == '#') continue;

        if (line == "ERROR") {
            expect.expect_error = true;
            continue;
        }

        const auto colon = line.find(':');
        if (colon == std::string::npos)
            throw std::runtime_error("malformed expectation line " +
                                     std::to_string(line_no) + ": " + line);

        const auto directive = line.substr(0, colon);
        auto needle = line.substr(colon + 1);
        const auto needle_start = needle.find_first_not_of(" \t");
        needle = needle_start == std::string::npos
                     ? ""
                     : needle.substr(needle_start);

        if (directive == "CONTAINS")
            expect.assertions.push_back(
                {.expect_present = true, .needle = needle});
        else if (directive == "NOT_CONTAINS")
            expect.assertions.push_back(
                {.expect_present = false, .needle = needle});
        else
            throw std::runtime_error("unknown directive at line " +
                                     std::to_string(line_no) + ": " +
                                     directive);
    }
    return expect;
}

} // namespace

auto main() -> int {
    try {
        const auto fixture = read_fixture_from_stdin();
        const auto expect = parse_expectations(fixture.expect_text);

        std::string json;
        bool threw = false;
        try {
            json = compile_to_json(fixture.score_source);
        } catch (const std::exception &e) {
            threw = true;
            if (!expect.expect_error) {
                std::cerr << "unexpected compile error: " << e.what() << "\n";
                return 1;
            }
        }

        if (expect.expect_error && !threw) {
            std::cerr << "expected a compile error but compilation succeeded: "
                      << json << "\n";
            return 1;
        }

        bool ok = true;
        for (const auto &a : expect.assertions)
            if (json.contains(a.needle) != a.expect_present) ok = false;

        std::cerr << json << "\n";
        if (!ok) return 1;

        std::cout << "OK\n";
        return 0;
    } catch (const std::exception &e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
}
