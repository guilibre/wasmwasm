#pragma once

#include <cstdint>
#include <optional>
#include <string>

namespace auth {

[[nodiscard]] auto hash_password(const std::string &password) -> std::string;
[[nodiscard]] auto verify_password(const std::string &password,
                                   const std::string &hash) -> bool;

[[nodiscard]] auto create_token(std::int64_t user_id) -> std::string;
[[nodiscard]] auto verify_token(const std::string &token)
    -> std::optional<std::int64_t>;

} // namespace auth
