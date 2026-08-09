#include "cors_middleware.hpp"
#include "jwt_middleware.hpp"
#include "routes_patches.hpp"
#include "routes_users.hpp"
#include <crow.h>
#include <crow/middlewares/cookie_parser.h>

auto main() -> int {
    crow::App<CorsAuth, crow::CookieParser, JwtAuth> app;

    routes::register_user_routes(app);
    routes::register_patch_routes(app);

    app.port(8080).multithreaded().run();
}
