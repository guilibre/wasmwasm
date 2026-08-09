#include "routes_patches.hpp"

#include "db.hpp"

#include <occi.h>

namespace routes {

namespace {

auto has_row(oracle::occi::ResultSet *rs) -> bool {
    return rs->next() != oracle::occi::ResultSet::END_OF_FETCH;
}

auto read_clob(oracle::occi::ResultSet *rs, uint index) -> std::string {
    auto clob = rs->getClob(index);
    clob.open(oracle::occi::OCCI_LOB_READONLY);
    const auto length = clob.length();
    std::string result(static_cast<std::size_t>(length), '\0');
    if (length > 0) {
        auto *stream = clob.getStream();
        stream->readBuffer(result.data(), length);
        clob.closeStream(stream);
    }
    clob.close();
    return result;
}

auto current_patch_data(db::Connection &connection, long long patch_id)
    -> std::optional<std::string> {
    auto stmt = connection.statement(
        "SELECT JSON_SERIALIZE(pv.data RETURNING CLOB) "
        "FROM patches p JOIN patch_versions pv ON pv.id = p.current_version_id "
        "WHERE p.id = :1");
    stmt->setInt(1, static_cast<int>(patch_id));
    const auto rs = db::query(stmt);
    if (!has_row(rs.get())) return std::nullopt;
    return read_clob(rs.get(), 1);
}

auto handle_create(const crow::request &req, const JwtAuth::context &ctx)
    -> crow::response {
    const auto body = crow::json::load(req.body);
    if (!body || !body.has("title") || !body.has("data")) {
        return {400, "expected {title, data}"};
    }

    const std::string title = body["title"].s();
    const std::string data = body["data"].s();
    const bool is_public = body.has("is_public") && body["is_public"].b();
    if (title.empty() || title.size() > 120) { return {400, "invalid title"}; }

    try {
        db::Connection connection;

        auto insert_patch = connection.statement(
            "INSERT INTO patches (user_id, title, is_public) VALUES "
            "(:1, :2, :3) RETURNING id INTO :4");
        insert_patch->setInt(1, static_cast<int>(ctx.user_id));
        insert_patch->setString(2, title);
        insert_patch->setInt(3, is_public ? 1 : 0);
        insert_patch->registerOutParam(4, oracle::occi::OCCIINT);
        insert_patch->executeUpdate();
        const auto patch_id = insert_patch->getInt(4);

        auto insert_version = connection.statement(
            "INSERT INTO patch_versions (patch_id, version_number, data) "
            "VALUES (:1, 1, JSON(:2)) RETURNING id INTO :3");
        insert_version->setInt(1, patch_id);
        insert_version->setString(2, data);
        insert_version->registerOutParam(3, oracle::occi::OCCIINT);
        insert_version->executeUpdate();
        const auto version_id = insert_version->getInt(3);

        auto update_current = connection.statement(
            "UPDATE patches SET current_version_id = :1 WHERE id = :2");
        update_current->setInt(1, version_id);
        update_current->setInt(2, patch_id);
        update_current->executeUpdate();

        connection.conn()->commit();

        crow::json::wvalue res;
        res["id"] = patch_id;
        res["title"] = title;
        res["is_public"] = is_public;
        res["version"] = 1;
        return {201, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to create patch: " << ex.what();
        return {500, "failed to create patch"};
    }
}

auto handle_list_mine(const JwtAuth::context &ctx) -> crow::response {
    try {
        db::Connection connection;

        auto stmt = connection.statement(
            "SELECT p.id, p.title, p.is_public, pv.version_number "
            "FROM patches p JOIN patch_versions pv ON pv.id = "
            "p.current_version_id "
            "WHERE p.user_id = :1 ORDER BY p.id");
        stmt->setInt(1, static_cast<int>(ctx.user_id));
        const auto rs = db::query(stmt);

        crow::json::wvalue::list items;
        while (has_row(rs.get())) {
            crow::json::wvalue item;
            item["id"] = rs->getInt(1);
            item["title"] = rs->getString(2);
            item["is_public"] = rs->getInt(3) == 1;
            item["version"] = rs->getInt(4);
            items.push_back(std::move(item));
        }

        crow::json::wvalue res;
        res["patches"] = std::move(items);
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to list patches: " << ex.what();
        return {500, "failed to list patches"};
    }
}

auto handle_get(long long patch_id, const JwtAuth::context &ctx)
    -> crow::response {
    try {
        db::Connection connection;

        auto stmt = connection.statement(
            "SELECT user_id, title, is_public FROM patches WHERE id = :1");
        stmt->setInt(1, static_cast<int>(patch_id));
        const auto rs = db::query(stmt);
        if (!has_row(rs.get())) { return {404, "patch not found"}; }

        const auto owner_id = rs->getInt(1);
        const auto title = rs->getString(2);
        const auto is_public = rs->getInt(3) == 1;
        if (owner_id != ctx.user_id && !is_public) {
            return {404, "patch not found"};
        }

        const auto data = current_patch_data(connection, patch_id);
        if (!data) { return {404, "patch not found"}; }

        crow::json::wvalue res;
        res["id"] = patch_id;
        res["title"] = title;
        res["is_public"] = is_public;
        res["data"] = crow::json::load(*data);
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to fetch patch: " << ex.what();
        return {500, "failed to fetch patch"};
    }
}

auto handle_update(long long patch_id, const crow::request &req,
                   const JwtAuth::context &ctx) -> crow::response {
    const auto body = crow::json::load(req.body);
    if (!body || !body.has("data")) { return {400, "expected {data}"}; }
    const auto data = body["data"].s();
    const auto has_title = body.has("title");
    const auto title = has_title ? body["title"].s() : std::string{};
    if (has_title && (title.empty() || title.size() > 120)) {
        return {400, "invalid title"};
    }

    try {
        db::Connection connection;

        auto owner_stmt =
            connection.statement("SELECT user_id FROM patches WHERE id = :1");
        owner_stmt->setInt(1, static_cast<int>(patch_id));
        const auto owner_rs = db::query(owner_stmt);
        if (!has_row(owner_rs.get())) { return {404, "patch not found"}; }
        if (owner_rs->getInt(1) != ctx.user_id) {
            return {403, "not the owner of this patch"};
        }

        auto max_version_stmt =
            connection.statement("SELECT MAX(version_number) FROM "
                                 "patch_versions WHERE patch_id = :1");
        max_version_stmt->setInt(1, static_cast<int>(patch_id));
        const auto max_version_rs = db::query(max_version_stmt);
        has_row(max_version_rs.get());
        const auto next_version = max_version_rs->getInt(1) + 1;

        auto insert_version = connection.statement(
            "INSERT INTO patch_versions (patch_id, version_number, data) "
            "VALUES (:1, :2, JSON(:3)) RETURNING id INTO :4");
        insert_version->setInt(1, static_cast<int>(patch_id));
        insert_version->setInt(2, next_version);
        insert_version->setString(3, data);
        insert_version->registerOutParam(4, oracle::occi::OCCIINT);
        insert_version->executeUpdate();
        const auto version_id = insert_version->getInt(4);

        if (has_title) {
            auto update_patch = connection.statement(
                "UPDATE patches SET current_version_id = :1, title = :2 "
                "WHERE id = :3");
            update_patch->setInt(1, version_id);
            update_patch->setString(2, title);
            update_patch->setInt(3, static_cast<int>(patch_id));
            update_patch->executeUpdate();
        } else {
            auto update_patch = connection.statement(
                "UPDATE patches SET current_version_id = :1 WHERE id = :2");
            update_patch->setInt(1, version_id);
            update_patch->setInt(2, static_cast<int>(patch_id));
            update_patch->executeUpdate();
        }

        connection.conn()->commit();

        crow::json::wvalue res;
        res["id"] = patch_id;
        res["version"] = next_version;
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to update patch: " << ex.what();
        return {500, "failed to update patch"};
    }
}

auto handle_delete(long long patch_id, const JwtAuth::context &ctx)
    -> crow::response {
    try {
        db::Connection connection;

        auto owner_stmt =
            connection.statement("SELECT user_id FROM patches WHERE id = :1");
        owner_stmt->setInt(1, static_cast<int>(patch_id));
        const auto owner_rs = db::query(owner_stmt);
        if (!has_row(owner_rs.get())) { return {404, "patch not found"}; }
        if (owner_rs->getInt(1) != ctx.user_id) {
            return {403, "not the owner of this patch"};
        }

        auto delete_stmt =
            connection.statement("DELETE FROM patches WHERE id = :1");
        delete_stmt->setInt(1, static_cast<int>(patch_id));
        delete_stmt->executeUpdate();
        connection.conn()->commit();

        return {204, ""};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to delete patch: " << ex.what();
        return {500, "failed to delete patch"};
    }
}

auto handle_list_versions(long long patch_id, const JwtAuth::context &ctx)
    -> crow::response {
    try {
        db::Connection connection;

        auto owner_stmt =
            connection.statement("SELECT user_id FROM patches WHERE id = :1");
        owner_stmt->setInt(1, static_cast<int>(patch_id));
        const auto owner_rs = db::query(owner_stmt);
        if (!has_row(owner_rs.get())) { return {404, "patch not found"}; }
        if (owner_rs->getInt(1) != ctx.user_id) {
            return {403, "not the owner of this patch"};
        }

        auto stmt = connection.statement(
            "SELECT version_number FROM patch_versions WHERE patch_id = :1 "
            "ORDER BY version_number");
        stmt->setInt(1, static_cast<int>(patch_id));
        const auto rs = db::query(stmt);

        crow::json::wvalue::list versions;
        while (has_row(rs.get())) versions.emplace_back(rs->getInt(1));

        crow::json::wvalue res;
        res["versions"] = std::move(versions);
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to list patch versions: " << ex.what();
        return {500, "failed to list patch versions"};
    }
}

auto handle_get_version(long long patch_id, long long version_number,
                        const JwtAuth::context &ctx) -> crow::response {
    try {
        db::Connection connection;

        auto owner_stmt =
            connection.statement("SELECT user_id FROM patches WHERE id = :1");
        owner_stmt->setInt(1, static_cast<int>(patch_id));
        const auto owner_rs = db::query(owner_stmt);
        if (!has_row(owner_rs.get())) { return {404, "patch not found"}; }
        if (owner_rs->getInt(1) != ctx.user_id) {
            return {403, "not the owner of this patch"};
        }

        auto stmt = connection.statement(
            "SELECT JSON_SERIALIZE(data RETURNING CLOB) FROM patch_versions "
            "WHERE patch_id = :1 AND version_number = :2");
        stmt->setInt(1, static_cast<int>(patch_id));
        stmt->setInt(2, static_cast<int>(version_number));
        const auto rs = db::query(stmt);
        if (!has_row(rs.get())) { return {404, "version not found"}; }

        crow::json::wvalue res;
        res["version"] = version_number;
        res["data"] = crow::json::load(read_clob(rs.get(), 1));
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to fetch patch version: " << ex.what();
        return {500, "failed to fetch patch version"};
    }
}

auto handle_list_public() -> crow::response {
    try {
        db::Connection connection;

        auto stmt = connection.statement(
            "SELECT p.id, p.title, u.username FROM patches p "
            "JOIN users u ON u.id = p.user_id "
            "WHERE p.is_public = 1 ORDER BY p.id");
        const auto rs = db::query(stmt);

        crow::json::wvalue::list items;
        while (has_row(rs.get())) {
            crow::json::wvalue item;
            item["id"] = rs->getInt(1);
            item["title"] = rs->getString(2);
            item["owner"] = rs->getString(3);
            items.push_back(std::move(item));
        }

        crow::json::wvalue res;
        res["patches"] = std::move(items);
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to list public patches: " << ex.what();
        return {500, "failed to list public patches"};
    }
}

auto handle_get_public(long long patch_id) -> crow::response {
    try {
        db::Connection connection;

        auto stmt = connection.statement(
            "SELECT title FROM patches WHERE id = :1 AND is_public = 1");
        stmt->setInt(1, static_cast<int>(patch_id));
        const auto rs = db::query(stmt);
        if (!has_row(rs.get())) { return {404, "patch not found"}; }
        const auto title = rs->getString(1);

        const auto data = current_patch_data(connection, patch_id);
        if (!data) { return {404, "patch not found"}; }

        crow::json::wvalue res;
        res["id"] = patch_id;
        res["title"] = title;
        res["data"] = crow::json::load(*data);
        return {200, res.dump()};
    } catch (const std::exception &ex) {
        CROW_LOG_ERROR << "failed to fetch public patch: " << ex.what();
        return {500, "failed to fetch public patch"};
    }
}

} // namespace

void register_patch_routes(
    crow::App<CorsAuth, crow::CookieParser, JwtAuth> &app) {
    CROW_ROUTE(app, "/patches")
        .methods(crow::HTTPMethod::Post)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)([&app](const crow::request &req) {
            return handle_create(req, app.get_context<JwtAuth>(req));
        });

    CROW_ROUTE(app, "/patches")
        .methods(crow::HTTPMethod::Get)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)([&app](const crow::request &req) {
            return handle_list_mine(app.get_context<JwtAuth>(req));
        });

    CROW_ROUTE(app, "/patches/<int>")
        .methods(crow::HTTPMethod::Get)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)(
            [&app](const crow::request &req, int patch_id) {
                return handle_get(patch_id, app.get_context<JwtAuth>(req));
            });

    CROW_ROUTE(app, "/patches/<int>")
        .methods(crow::HTTPMethod::Put)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(
            app, JwtAuth)([&app](const crow::request &req, int patch_id) {
            return handle_update(patch_id, req, app.get_context<JwtAuth>(req));
        });

    CROW_ROUTE(app, "/patches/<int>")
        .methods(crow::HTTPMethod::Delete)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)(
            [&app](const crow::request &req, int patch_id) {
                return handle_delete(patch_id, app.get_context<JwtAuth>(req));
            });

    CROW_ROUTE(app, "/patches/<int>/versions")
        .methods(crow::HTTPMethod::Get)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)(
            [&app](const crow::request &req, int patch_id) {
                return handle_list_versions(patch_id,
                                            app.get_context<JwtAuth>(req));
            });

    CROW_ROUTE(app, "/patches/<int>/versions/<int>")
        .methods(crow::HTTPMethod::Get)
        // NOLINTNEXTLINE
        .CROW_MIDDLEWARES(app, JwtAuth)(
            [&app](const crow::request &req, int patch_id, int version) {
                return handle_get_version(patch_id, version,
                                          app.get_context<JwtAuth>(req));
            });

    CROW_ROUTE(app, "/public/patches").methods(crow::HTTPMethod::Get)([] {
        return handle_list_public();
    });

    CROW_ROUTE(app, "/public/patches/<int>")
        .methods(crow::HTTPMethod::Get)(
            [](int patch_id) { return handle_get_public(patch_id); });
}

} // namespace routes
