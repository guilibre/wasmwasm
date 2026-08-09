#include "routes_users.hpp"

#include "auth.hpp"
#include "db.hpp"

#include <cstdlib>
#include <cstring>
#include <occi.h>

namespace routes {

namespace {

auto cookie_secure() -> bool {
    const auto *value = std::getenv("COOKIE_SECURE");
    return value == nullptr || std::strcmp(value, "false") != 0;
}

auto build_session_cookie(const std::string &token)
    -> crow::CookieParser::Cookie {
    auto cookie = crow::CookieParser::Cookie{SESSION_COOKIE_NAME, token};
    cookie.path("/")
        .max_age(60LL * 60 * 24 * 7)
        .httponly()
        .same_site(crow::CookieParser::Cookie::SameSitePolicy::Lax);
    if (cookie_secure()) { cookie.secure(); }
    return cookie;
}

auto build_expired_cookie() -> crow::CookieParser::Cookie {
    auto cookie = crow::CookieParser::Cookie{SESSION_COOKIE_NAME, ""};
    cookie.path("/").max_age(0).httponly().same_site(
        crow::CookieParser::Cookie::SameSitePolicy::Lax);
    if (cookie_secure()) { cookie.secure(); }
    return cookie;
}

auto handle_register(const crow::request &req) -> crow::response {
    const auto body = crow::json::load(req.body);
    if (!body || !body.has("username") || !body.has("password")) {
        return {400, "expected {username, password}"};
    }

    const std::string username = body["username"].s();
    const std::string password = body["password"].s();
    if (username.empty() || username.size() > 50 || password.size() < 8) {
        return {400, "invalid username or password"};
    }

    try {
        db::Connection connection;
        const auto hash = auth::hash_password(password);

        auto stmt = connection.statement(
            "INSERT INTO users (username, password_hash) VALUES (:1, :2) "
            "RETURNING id INTO :3");
        stmt->setString(1, username);
        stmt->setString(2, hash);
        stmt->registerOutParam(3, oracle::occi::OCCIINT);
        stmt->executeUpdate();
        const auto user_id = stmt->getInt(3);
        connection.conn()->commit();

        crow::response res{201};
        res.add_header(
            "Set-Cookie",
            build_session_cookie(auth::create_token(user_id)).dump());
        crow::json::wvalue json_res;
        json_res["id"] = user_id;
        json_res["username"] = username;
        res.body = json_res.dump();
        return res;
    } catch (const oracle::occi::SQLException &ex) {
        if (ex.getErrorCode() == 1) { return {409, "username already taken"}; }
        CROW_LOG_ERROR << "failed to register user: " << ex.what();
        return {500, "failed to register user"};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to register user: " << ex.what();
        return {500, "failed to register user"};
    }
}

auto handle_login(const crow::request &req) -> crow::response {
    const auto body = crow::json::load(req.body);
    if (!body || !body.has("username") || !body.has("password")) {
        return {400, "expected {username, password}"};
    }

    const auto username = body["username"].s();
    const auto password = body["password"].s();

    try {
        db::Connection connection;

        auto stmt = connection.statement(
            "SELECT id, password_hash FROM users WHERE username = :1");
        stmt->setString(1, username);
        const auto rs = db::query(stmt);

        if (rs->next() == oracle::occi::ResultSet::END_OF_FETCH) {
            return {401, "invalid credentials"};
        }

        const auto user_id = rs->getInt(1);
        const auto password_hash = rs->getString(2);
        if (!auth::verify_password(password, password_hash)) {
            return {401, "invalid credentials"};
        }

        crow::response res{200};
        res.add_header(
            "Set-Cookie",
            build_session_cookie(auth::create_token(user_id)).dump());
        crow::json::wvalue json_res;
        json_res["id"] = user_id;
        json_res["username"] = std::string{username};
        res.body = json_res.dump();
        return res;
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to log in: " << ex.what();
        return {500, "failed to log in"};
    }
}

auto handle_logout() -> crow::response {
    crow::response res{204};
    res.add_header("Set-Cookie", build_expired_cookie().dump());
    return res;
}

auto handle_me(std::int64_t user_id) -> crow::response {
    try {
        db::Connection connection;
        auto stmt =
            connection.statement("SELECT username FROM users WHERE id = :1");
        stmt->setInt(1, static_cast<int>(user_id));
        const auto rs = db::query(stmt);

        if (rs->next() == oracle::occi::ResultSet::END_OF_FETCH) {
            return {401, "invalid session"};
        }

        crow::json::wvalue json_res;
        json_res["id"] = user_id;
        json_res["username"] = rs->getString(1);
        return {200, json_res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to fetch current user: " << ex.what();
        return {500, "failed to fetch current user"};
    }
}

} // namespace

void register_user_routes(
    crow::App<CorsAuth, crow::CookieParser, JwtAuth> &app) {
    CROW_ROUTE(app, "/register")
        .methods(crow::HTTPMethod::Post)(handle_register);

    CROW_ROUTE(app, "/login").methods(crow::HTTPMethod::Post)(handle_login);

    CROW_ROUTE(app, "/logout").methods(crow::HTTPMethod::Post)(handle_logout);

    CROW_ROUTE(app, "/me")
        .methods(crow::HTTPMethod::Get)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)([&app](const crow::request &req) {
            auto &ctx = app.get_context<JwtAuth>(req);
            return handle_me(ctx.user_id);
        });
}

} // namespace routes
