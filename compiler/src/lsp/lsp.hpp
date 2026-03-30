#pragma once

extern "C" auto lsp_diagnostics(const char *src) -> const char *;
extern "C" auto lsp_tokens(const char *src) -> const char *;
extern "C" auto lsp_completions(const char *src, int line, int col) -> const
    char *;
extern "C" auto lsp_hover(const char *src, int line, int col) -> const char *;
