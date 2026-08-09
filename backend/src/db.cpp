#include "db.hpp"

#include <cstdlib>
#include <stdexcept>

namespace db {

namespace {

auto getenv_required(const char *name) -> std::string {
    const auto *value = std::getenv(name);
    if (value == nullptr) {
        throw std::runtime_error(std::string{"missing environment variable "} +
                                 name);
    }
    return value;
}

} // namespace

Connection::Connection()
    : env_{oracle::occi::Environment::createEnvironment(),
           oracle::occi::Environment::terminateEnvironment},
      conn_{nullptr, [](oracle::occi::Connection *) {}} {
    const auto user_name = getenv_required("DB_USER");
    const auto user_password = getenv_required("DB_PASSWORD");
    const auto connection_string = getenv_required("DB_CONNECT_STRING");

    auto *raw_env = env_.get();
    conn_ = ConnPtr{
        raw_env->createConnection(user_name, user_password, connection_string),
        [raw_env](oracle::occi::Connection *c) {
            raw_env->terminateConnection(c);
        }};
}

auto Connection::conn() -> oracle::occi::Connection * { return conn_.get(); }

auto Connection::statement(const std::string &sql) -> StmtPtr {
    auto *raw_conn = conn_.get();
    return StmtPtr{raw_conn->createStatement(sql),
                   [raw_conn](oracle::occi::Statement *s) {
                       raw_conn->terminateStatement(s);
                   }};
}

auto query(StmtPtr &stmt) -> ResultSetPtr {
    auto *raw_stmt = stmt.get();
    return ResultSetPtr{raw_stmt->executeQuery(),
                        [raw_stmt](oracle::occi::ResultSet *rs) {
                            raw_stmt->closeResultSet(rs);
                        }};
}

} // namespace db
