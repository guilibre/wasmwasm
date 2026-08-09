#pragma once

#include <crow.h>
#include <cstdlib>
#include <string>

struct CorsAuth {
    struct context {};

    void before_handle(crow::request & /*request*/,
                       crow::response & /*response*/, context & /*context*/) {}

    void after_handle(crow::request & /*request*/, crow::response &res,
                      context & /*context*/) {
        res.add_header("Access-Control-Allow-Origin", frontend_origin());
        res.add_header("Access-Control-Allow-Credentials", "true");
        res.add_header("Access-Control-Allow-Headers", "Content-Type");
        res.add_header("Access-Control-Allow-Methods",
                       "GET, POST, PUT, DELETE, OPTIONS");
    }

  private:
    static auto frontend_origin() -> std::string {
        const auto *origin = std::getenv("FRONTEND_ORIGIN");
        return origin != nullptr ? origin : "http://localhost:5173";
    }
};
