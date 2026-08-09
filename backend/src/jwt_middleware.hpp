#pragma once

#include "auth.hpp"
#include <crow.h>
#include <crow/middlewares/cookie_parser.h>
#include <cstdint>

inline constexpr const char *SESSION_COOKIE_NAME = "session";

struct JwtAuth : crow::ILocalMiddleware {
    struct context {
        std::int64_t user_id{};
    };

    template <typename AllContext>
    void before_handle(crow::request &req, crow::response &res, context &ctx,
                       AllContext &all_ctx) {
        auto &cookie_ctx = all_ctx.template get<crow::CookieParser>();
        const auto token = cookie_ctx.get_cookie(SESSION_COOKIE_NAME);
        if (token.empty()) {
            res.code = 401;
            res.end("missing session cookie");
            return;
        }

        const auto user_id = auth::verify_token(token);
        if (!user_id) {
            res.code = 401;
            res.end("invalid or expired token");
            return;
        }

        ctx.user_id = *user_id;
        (void)req;
    }

    void after_handle(crow::request & /*request*/,
                      crow::response & /*response*/, context & /*context*/) {}
};
