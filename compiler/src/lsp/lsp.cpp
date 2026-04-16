#include "lsp.hpp"

#include "../ast/ast.hpp"
#include "../parser/parser.hpp"
#include "../parser/tokenizer.hpp"
#include "../types/type.hpp"
#include "../types/type_inference.hpp"

#include <algorithm>
#include <array>
#include <cstring>
#include <string>
#include <type_traits>
#include <unordered_set>
#include <variant>
#include <vector>

namespace {

auto json_escape(const std::string &s) -> std::string {
    std::string out;
    out.reserve(s.size());
    for (char c : s) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else if (c == '\n')
            out += "\\n";
        else if (c == '\r')
            out += "\\r";
        else
            out += c;
    }
    return out;
}

auto type_to_string(const TypePtr &type) -> std::string {
    if (!type) return "?";
    return std::visit(
        [&](const auto &node) -> std::string {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeBase>) {
                switch (node.kind) {
                case BaseTypeKind::Float:
                    return "Float";
                case BaseTypeKind::Int:
                    return "Int";
                case BaseTypeKind::Bool:
                    return "Bool";
                case BaseTypeKind::Void:
                    return "Void";
                }
                return "?";
            }
            if constexpr (std::is_same_v<T, TypeFun>) {
                auto param = type_to_string(node.param);
                if (std::holds_alternative<TypeFun>(node.param->node))
                    param = "(" + param + ")";
                return param + " -> " + type_to_string(node.result);
            }
            if constexpr (std::is_same_v<T, TypeVar>) {
                return "t" + std::to_string(node.id);
            }
            return "?";
        },
        type->node);
}

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>> {
    auto int_type = Type::make<TypeBase>(BaseTypeKind::Int);
    auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
    auto void_type = Type::make<TypeBase>(BaseTypeKind::Void);
    auto int_to_float = Type::make<TypeFun>(int_type, float_type);
    auto float_to_float = Type::make<TypeFun>(float_type, float_type);
    return {{
        {"PI", float_type},
        {"TIME", float_type},
        {"SAMPLE_RATE", float_type},
        {"OUT", float_type},
        {"cos", float_to_float},
        {"sin", float_to_float},
        {"sign", float_to_float},
        {"fract", float_to_float},
        {"clip", float_to_float},
        {"exp", float_to_float},
        {"buffer",
         Type::make<TypeFun>(
             float_type,
             Type::make<TypeFun>(
                 int_type, Type::make<TypeFun>(int_to_float, void_type)))},
    }};
}

auto find_node_at(const ExprPtr &expr, size_t line, size_t col)
    -> const Expr * {
    if (!expr) return nullptr;
    return std::visit(
        [&](const auto &node) -> const Expr * {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Variable>) {
                const auto &tok = node.name;
                if (tok.line == line && col >= tok.column &&
                    col < tok.column + tok.lexeme.size())
                    return expr.get();
                return nullptr;
            }
            if constexpr (std::is_same_v<T, Literal>) {
                const auto &tok = node.value;
                if (tok.line == line && col >= tok.column &&
                    col < tok.column + tok.lexeme.size())
                    return expr.get();
                return nullptr;
            }
            if constexpr (std::is_same_v<T, Lambda>) {
                const auto &param = node.parameter;
                if (param.line == line && col >= param.column &&
                    col < param.column + param.lexeme.size())
                    return expr.get();
                return find_node_at(node.body, line, col);
            }
            if constexpr (std::is_same_v<T, Bind>) {
                return find_node_at(node.value, line, col);
            }
            if constexpr (std::is_same_v<T, BufferWrite>) {
                return find_node_at(node.value, line, col);
            }
            if constexpr (std::is_same_v<T, Block>) {
                for (const auto &child : node.expressions) {
                    auto *hit = find_node_at(child, line, col);
                    if (hit) return hit;
                }
                return nullptr;
            }
            if constexpr (std::is_same_v<T, BinaryOp>) {
                auto *hit = find_node_at(node.left, line, col);
                return hit ? hit : find_node_at(node.right, line, col);
            }
            if constexpr (std::is_same_v<T, UnaryOp>) {
                return find_node_at(node.expr, line, col);
            }
            if constexpr (std::is_same_v<T, Call>) {
                auto *hit = find_node_at(node.callee, line, col);
                return hit ? hit : find_node_at(node.argument, line, col);
            }
            if constexpr (std::is_same_v<T, BufferCtor>) {
                return find_node_at(node.init_fn, line, col);
            }
            return nullptr;
        },
        expr->node);
}

void collect_user_defs(const ExprPtr &expr, std::vector<std::string> &defs) {
    if (!expr) return;
    std::visit(
        [&](const auto &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Bind>) {
                defs.push_back(node.name.lexeme);
                collect_user_defs(node.value, defs);
            }
            if constexpr (std::is_same_v<T, BufferWrite>) {
                collect_user_defs(node.value, defs);
            }
            if constexpr (std::is_same_v<T, Block>) {
                for (const auto &child : node.expressions)
                    collect_user_defs(child, defs);
            }
            if constexpr (std::is_same_v<T, Lambda>) {
                collect_user_defs(node.body, defs);
            }
            if constexpr (std::is_same_v<T, Call>) {
                collect_user_defs(node.callee, defs);
                collect_user_defs(node.argument, defs);
            }
            if constexpr (std::is_same_v<T, BinaryOp>) {
                collect_user_defs(node.left, defs);
                collect_user_defs(node.right, defs);
            }
            if constexpr (std::is_same_v<T, UnaryOp>) {
                collect_user_defs(node.expr, defs);
            }
        },
        expr->node);
}

struct CommentStart {
    size_t line;
    size_t col;
};

auto scan_comments(const char *src) -> std::vector<CommentStart> {
    std::vector<CommentStart> out;
    size_t line = 1;
    size_t col = 1;
    for (const char *p = src; *p != 0; ++p) {
        if (*p == '#') {
            out.push_back({line, col});
            while ((*p != 0) && *p != '\n')
                ++p;
            if (*p == 0) break;
        }
        if (*p == '\n') {
            ++line;
            col = 1;
        } else
            ++col;
    }
    return out;
}

auto is_in_comment(const std::vector<CommentStart> &comments, size_t line,
                   size_t col) -> bool {
    return std::ranges::any_of(comments, [&](const auto &c) {
        return c.line == line && col >= c.col;
    });
}

auto line_len_from(const char *src, size_t target_line, size_t start_col)
    -> size_t {
    size_t line = 1;
    size_t col = 1;
    size_t len = 0;
    for (const char *p = src; *p != 0; ++p) {
        if (line == target_line && col >= start_col) {
            if (*p == '\n') break;
            ++len;
        }
        if (*p == '\n') {
            ++line;
            col = 1;
        } else
            ++col;
    }
    return len;
}

} // namespace

extern "C" auto lsp_diagnostics(const char *src) -> const char * {
    static std::array<char, 131072> s_result;
    Tokenizer main_tok(src);
    Parser main_parser(main_tok);
    auto main_result = main_parser.parse_code();
    if (!main_result) {
        const auto &err = main_result.error();
        std::string error_json = R"([{"msg":")" + json_escape(err.msg) +
                                 R"(","line":)" + std::to_string(err.line) +
                                 ",\"col\":" + std::to_string(err.col) +
                                 R"(,"severity":"error"}])";
        if (error_json.size() >= s_result.size())
            std::strcpy(s_result.data(), "[]");
        else
            std::strcpy(s_result.data(), error_json.c_str());
        return s_result.data();
    }

    try {
        auto env = make_builtin_env();
        Substitution subst;
        TypeGenerator gen;
        infer_expr(*main_result, env, subst, gen);
    } catch (const std::exception &e) {
        std::string error_json = R"([{"msg":")" + json_escape(e.what()) +
                                 "\",\"line\":0,\"col\":0,"
                                 "\"severity\":\"error\"}]";
        if (error_json.size() >= s_result.size())
            std::strcpy(s_result.data(), "[]");
        else
            std::strcpy(s_result.data(), error_json.c_str());
        return s_result.data();
    }

    std::strcpy(s_result.data(), "[]");
    return s_result.data();
}

extern "C" auto lsp_tokens(const char *src) -> const char * {
    static std::array<char, 131072> s_result;
    static const std::unordered_set<std::string> builtins{
        "sin", "cos", "sign", "fract",       "clip",
        "exp", "PI",  "TIME", "SAMPLE_RATE", "OUT",
    };

    auto comments = scan_comments(src);

    std::string json;
    json.reserve(4096);
    json += '[';
    bool first = true;

    auto emit = [&](size_t line, size_t col, size_t len, const char *type) {
        if (!first) json += ',';
        first = false;
        json += "{\"line\":" + std::to_string(line) +
                ",\"col\":" + std::to_string(col) +
                ",\"len\":" + std::to_string(len) + R"(,"type":")" +
                std::string(type) + "\"}";
    };

    for (const auto &c : comments) {
        size_t len = line_len_from(src, c.line, c.col);
        emit(c.line, c.col, len, "comment");
    }

    Tokenizer tok(src);
    while (!tok.is_done()) {
        Token t = tok.next();
        if (t.kind == TokenKind::Eof || t.kind == TokenKind::Eol ||
            t.kind == TokenKind::Invalid)
            continue;
        if (is_in_comment(comments, t.line, t.column)) continue;

        size_t len = t.lexeme.size();
        switch (t.kind) {
        case TokenKind::Number:
            emit(t.line, t.column, len, "number");
            break;
        case TokenKind::Identifier:
            if (t.lexeme == "delay")
                emit(t.line, t.column, len, "keyword");
            else if (builtins.contains(t.lexeme))
                emit(t.line, t.column, len, "function");
            else
                emit(t.line, t.column, len, "variable");
            break;
        case TokenKind::Additive:
        case TokenKind::Multiplicative:
        case TokenKind::Eq:
        case TokenKind::At:
        case TokenKind::LeftArrow:
        case TokenKind::Period:
            emit(t.line, t.column, len, "operator");
            break;
        default:
            break;
        }
    }

    json += ']';

    if (json.size() >= s_result.size())
        std::strcpy(s_result.data(), "[]");
    else
        std::strcpy(s_result.data(), json.c_str());
    return s_result.data();
}

extern "C" auto lsp_completions(const char *src, int /*line*/, int /*col*/)
    -> const char * {
    static std::array<char, 131072> s_result;
    struct BuiltinItem {
        const char *label;
        const char *detail;
        const char *kind;
    };
    static const std::array<BuiltinItem, 11> builtins = {{
        {.label = "sin", .detail = "Float -> Float", .kind = "function"},
        {.label = "cos", .detail = "Float -> Float", .kind = "function"},
        {.label = "sign", .detail = "Float -> Float", .kind = "function"},
        {.label = "fract", .detail = "Float -> Float", .kind = "function"},
        {.label = "clip", .detail = "Float -> Float", .kind = "function"},
        {.label = "exp", .detail = "Float -> Float", .kind = "function"},
        {.label = "PI", .detail = "Float", .kind = "constant"},
        {.label = "TIME", .detail = "Float", .kind = "constant"},
        {.label = "SAMPLE_RATE", .detail = "Float", .kind = "constant"},
        {.label = "OUT", .detail = "Float", .kind = "constant"},
        {.label = "delay",
         .detail = "Int -> (Int -> Float) -> Buffer",
         .kind = "keyword"},
    }};

    std::vector<std::string> user_defs;
    Tokenizer tok(src);
    Parser parser(tok);
    auto result = parser.parse_code();
    if (result) collect_user_defs(*result, user_defs);

    std::string json;
    json.reserve(2048);
    json += '[';
    bool first = true;

    auto emit = [&](const char *label, const char *detail, const char *kind) {
        if (!first) json += ',';
        first = false;
        json += R"({"label":")" + std::string(label) + R"(","detail":")" +
                std::string(detail) + R"(","kind":")" + std::string(kind) +
                "\"}";
    };

    for (const auto &b : builtins)
        emit(b.label, b.detail, b.kind);

    std::unordered_set<std::string> seen;
    for (const auto &b : builtins)
        seen.insert(b.label);
    for (const auto &name : user_defs) {
        if (!seen.contains(name)) {
            seen.insert(name);
            emit(name.c_str(), "Float", "variable");
        }
    }

    json += ']';

    if (json.size() >= s_result.size())
        std::strcpy(s_result.data(), "[]");
    else
        std::strcpy(s_result.data(), json.c_str());
    return s_result.data();
}

extern "C" auto lsp_hover(const char *src, int line, int col) -> const char * {
    static std::array<char, 131072> s_result;
    Tokenizer main_tok(src);
    Parser main_parser(main_tok);
    auto main_result = main_parser.parse_code();
    if (!main_result) {
        std::strcpy(s_result.data(), "null");
        return s_result.data();
    }

    try {
        auto env = make_builtin_env();
        Substitution subst;
        TypeGenerator gen;
        infer_expr(*main_result, env, subst, gen);
    } catch (...) {
        std::strcpy(s_result.data(), "null");
        return s_result.data();
    }

    auto uline = static_cast<size_t>(line);
    auto ucol = static_cast<size_t>(col);

    const Expr *hit = find_node_at(*main_result, uline, ucol);

    if ((hit == nullptr) || !hit->type) {
        std::strcpy(s_result.data(), "null");
        return s_result.data();
    }

    std::string type_str = type_to_string(hit->type);
    std::string json = R"({"type":")" + json_escape(type_str) + R"("})";
    if (json.size() >= s_result.size())
        std::strcpy(s_result.data(), "null");
    else
        std::strcpy(s_result.data(), json.c_str());
    return s_result.data();
}
