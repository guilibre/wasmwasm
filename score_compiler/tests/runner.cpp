#include "ast/binop_eval.hpp"
#include "compile.hpp"
#include "diagnostics.hpp"
#include "parser/parser.hpp"
#include "resolve/resolver.hpp"
#include <iostream>
#include <optional>
#include <sstream>
#include <string>

namespace {

struct StdinFixture {
    std::string score_source;
    std::string expect_text;
};

auto read_fixture_from_stdin() -> StdinFixture {
    std::string len_line;
    if (!std::getline(std::cin, len_line))
        throw std::runtime_error("stdin: missing score-source length header");

    size_t src_len = 0;
    try {
        src_len = std::stoul(len_line);
    } catch (const std::exception &) {
        throw std::runtime_error("stdin: invalid score-source length header: " +
                                 len_line);
    }

    std::string score_source(src_len, '\0');
    std::cin.read(score_source.data(), static_cast<std::streamsize>(src_len));
    if (static_cast<size_t>(std::cin.gcount()) != src_len)
        throw std::runtime_error("stdin: truncated score source");

    std::ostringstream rest;
    rest << std::cin.rdbuf();
    return {.score_source = std::move(score_source), .expect_text = rest.str()};
}

struct Expectation {
    std::optional<size_t> line;
    std::optional<size_t> col;
    std::string message_contains;
};

auto parse_expectations(const std::string &text) -> Expectation {
    Expectation exp;
    std::istringstream ss(text);
    std::string line;
    while (std::getline(ss, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (line.empty()) continue;

        const auto colon = line.find(':');
        if (colon == std::string::npos)
            throw std::runtime_error("malformed expectation line: " + line);

        const auto directive = line.substr(0, colon);
        auto value = line.substr(colon + 1);
        const auto value_start = value.find_first_not_of(" \t");
        value =
            value_start == std::string::npos ? "" : value.substr(value_start);

        if (directive == "LINE")
            exp.line = std::stoul(value);
        else if (directive == "COL")
            exp.col = std::stoul(value);
        else if (directive == "CONTAINS")
            exp.message_contains = value;
        else
            throw std::runtime_error("unknown directive: " + directive);
    }
    return exp;
}

struct Failure {
    std::string message;
    size_t line;
    size_t col;
};

// Mirrors exactly what score_compiler/app/bindings.cpp and
// score_compiler/src/lsp/lsp.cpp do: catch the three specific compiler
// exception types, normalize their 1-based position via the shared
// diagnostics.hpp helper, and treat everything else as a plain failure.
auto try_compile(const std::string &source) -> std::optional<Failure> {
    try {
        (void)compile_to_json(source);
        return std::nullopt;
    } catch (const ParseException &e) {
        const auto [line, col] = normalize_position(e.line, e.col);
        return Failure{.message = e.what(), .line = line, .col = col};
    } catch (const ResolveException &e) {
        const auto [line, col] = normalize_position(e.line, e.col);
        return Failure{.message = e.what(), .line = line, .col = col};
    } catch (const FoldException &e) {
        const auto [line, col] = normalize_position(e.line, e.col);
        return Failure{.message = e.what(), .line = line, .col = col};
    }
}

} // namespace

auto main() -> int {
    try {
        const auto fixture = read_fixture_from_stdin();
        const auto expect = parse_expectations(fixture.expect_text);

        const auto failure = try_compile(fixture.score_source);
        if (!failure) {
            std::cerr << "expected compilation to fail, but it succeeded\n";
            return 1;
        }

        bool ok = true;
        if (expect.line && *expect.line != failure->line) {
            std::cerr << "line mismatch: expected " << *expect.line << ", got "
                      << failure->line << "\n";
            ok = false;
        }
        if (expect.col && *expect.col != failure->col) {
            std::cerr << "col mismatch: expected " << *expect.col << ", got "
                      << failure->col << "\n";
            ok = false;
        }
        if (!expect.message_contains.empty() &&
            failure->message.find(expect.message_contains) ==
                std::string::npos) {
            std::cerr << "message mismatch: expected to contain '"
                      << expect.message_contains << "', got '"
                      << failure->message << "'\n";
            ok = false;
        }

        if (!ok) return 1;
        std::cout << "OK\n";
        return 0;
    } catch (const std::exception &e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
}
