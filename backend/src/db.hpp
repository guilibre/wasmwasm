#pragma once

#include <functional>
#include <memory>
#include <occi.h>
#include <string>

namespace db {

using EnvPtr = std::unique_ptr<oracle::occi::Environment,
                               void (*)(oracle::occi::Environment *)>;
using ConnPtr =
    std::unique_ptr<oracle::occi::Connection,
                    std::function<void(oracle::occi::Connection *)>>;
using StmtPtr = std::unique_ptr<oracle::occi::Statement,
                                std::function<void(oracle::occi::Statement *)>>;
using ResultSetPtr =
    std::unique_ptr<oracle::occi::ResultSet,
                    std::function<void(oracle::occi::ResultSet *)>>;

class Connection {
    EnvPtr env_;
    ConnPtr conn_;

  public:
    Connection();

    [[nodiscard]] auto conn() -> oracle::occi::Connection *;
    [[nodiscard]] auto statement(const std::string &sql) -> StmtPtr;
};

[[nodiscard]] auto query(StmtPtr &stmt) -> ResultSetPtr;

} // namespace db
