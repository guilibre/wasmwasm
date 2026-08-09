#include "auth.hpp"

#include <chrono>
#include <cstdlib>
#include <jwt-cpp/jwt.h>
#include <sodium.h>
#include <stdexcept>

namespace auth {

namespace {

auto jwt_secret() -> std::string {
    const auto *secret = std::getenv("JWT_SECRET");
    if (secret == nullptr)
        throw std::runtime_error("missing environment variable JWT_SECRET");

    return secret;
}

void ensure_sodium_init() {
    if (sodium_init() < 0)
        throw std::runtime_error("failed to initialize libsodium");
}

} // namespace

auto hash_password(const std::string &password) -> std::string {
    ensure_sodium_init();

    std::string hash(crypto_pwhash_STRBYTES, '\0');
    if (crypto_pwhash_str(hash.data(), password.c_str(), password.size(),
                          crypto_pwhash_OPSLIMIT_INTERACTIVE,
                          crypto_pwhash_MEMLIMIT_INTERACTIVE) != 0)
        throw std::runtime_error("out of memory while hashing password");

    hash.resize(std::char_traits<char>::length(hash.c_str()));
    return hash;
}

auto verify_password(const std::string &password, const std::string &hash)
    -> bool {
    ensure_sodium_init();
    return crypto_pwhash_str_verify(hash.c_str(), password.c_str(),
                                    password.size()) == 0;
}

auto create_token(std::int64_t user_id) -> std::string {
    const auto now = std::chrono::system_clock::now();
    return jwt::create()
        .set_type("JWT")
        .set_issued_at(now)
        .set_expires_at(now + std::chrono::hours{24 * 7})
        .set_payload_claim("sub", jwt::claim(std::to_string(user_id)))
        .sign(jwt::algorithm::hs256{jwt_secret()});
}

auto verify_token(const std::string &token) -> std::optional<std::int64_t> {
    try {
        const auto decoded = jwt::decode(token);
        jwt::verify()
            .allow_algorithm(jwt::algorithm::hs256{jwt_secret()})
            .with_type("JWT")
            .verify(decoded);
        return std::stoll(decoded.get_payload_claim("sub").as_string());
    } catch (const std::exception &) { return std::nullopt; }
}

} // namespace auth
