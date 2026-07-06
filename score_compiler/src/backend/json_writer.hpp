#pragma once

#include <string>

[[nodiscard]] inline auto json_escape(const std::string &text) -> std::string {
    std::string out;
    out.reserve(text.size() + 2);
    for (char c : text) {
        switch (c) {
        case '"':
            out += "\\\"";
            break;
        case '\\':
            out += "\\\\";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\t':
            out += "\\t";
            break;
        default:
            out += c;
        }
    }
    return out;
}

[[nodiscard]] inline auto json_string(const std::string &text) -> std::string {
    return "\"" + json_escape(text) + "\"";
}
