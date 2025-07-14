#pragma once

#include "../ast/ast.hpp"
#include "type.hpp"

#include <cstddef>
#include <unordered_map>

class TypeGenerator {
    size_t current_type = 0;

  public:
    auto fresh_type_var() -> TypePtr {
        return Type::make<TypeVar>(current_type++);
    }
};

using Substitution = std::unordered_map<size_t, TypePtr>;

auto occurs_in(size_t var_id, const TypePtr &type) -> bool;

auto apply_subst(const Substitution &subst, const TypePtr &type) -> TypePtr;

void unify(const TypePtr &a, const TypePtr &b, Substitution &subst);

auto infer_expr(const ExprPtr &expr,
                std::unordered_map<std::string_view, TypePtr> &env,
                Substitution &subst, TypeGenerator &gen) -> TypePtr;