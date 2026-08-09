#pragma once

#include "cors_middleware.hpp"
#include "jwt_middleware.hpp"
#include <crow.h>
#include <crow/middlewares/cookie_parser.h>

namespace routes {

void register_patch_routes(
    crow::App<CorsAuth, crow::CookieParser, JwtAuth> &app);

} // namespace routes
