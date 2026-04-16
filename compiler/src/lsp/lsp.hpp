#pragma once

#include <string>

auto lsp_diagnostics(const std::string &src) -> std::string;
auto lsp_tokens(const std::string &src) -> std::string;
auto lsp_completions(const std::string &src, int line, int col) -> std::string;
auto lsp_hover(const std::string &src, int line, int col) -> std::string;
