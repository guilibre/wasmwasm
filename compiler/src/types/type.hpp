#pragma once

#include "binaryen-c.h"
#include <memory>
#include <sys/types.h>
#include <variant>

struct Type;

using TypePtr = std::shared_ptr<Type>;

enum struct BaseTypeKind : u_int8_t { Bool, Float, Int, Void };

struct TypeBase {
    BaseTypeKind kind;
};

struct TypeFun {
    TypePtr param;
    TypePtr result;
};

struct TypeVar {
    size_t id;
};

using TypeNode = std::variant<TypeBase, TypeFun, TypeVar>;

struct Type {
    TypeNode node;

    explicit Type(TypeNode n) : node(std::move(n)) {}

    template <typename T, typename... Args>
    static auto make(Args &&...args) -> TypePtr {
        return std::make_shared<Type>(
            Type(TypeNode{T{std::forward<Args>(args)...}}));
    }

    auto to_binaryen_type() -> BinaryenType;
};
