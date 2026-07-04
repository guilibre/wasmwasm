#pragma once

#include "ast/ast.hpp"
#include "type.hpp"
#include <cstddef>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

struct TypeError : std::runtime_error {
    SourcePos pos;
    TypeError(const std::string &msg, SourcePos pos)
        : std::runtime_error(msg), pos(pos) {}
};

class TypeGenerator {
    size_t current_type = 0;

  public:
    auto fresh_type_var() -> TypePtr {
        return Type::make<TypeVar>(current_type++);
    }
};

using Substitution = std::unordered_map<size_t, TypePtr>;

auto scalar_kind_of(const TypePtr &t) -> BaseTypeKind;

auto scalar_kinds_of(const TypePtr &t) -> std::vector<BaseTypeKind>;

auto arity_of(const TypePtr &t) -> size_t;

auto occurs_in(size_t var_id, const TypePtr &type) -> bool;

auto apply_subst(const Substitution &subst, const TypePtr &type) -> TypePtr;

void unify(const TypePtr &a, const TypePtr &b, Substitution &subst,
           SourcePos pos);

void pre_register_toplevel(
    const ExprPtr &program,
    std::vector<std::unordered_map<std::string, TypePtr>> &env);

void infer_expr(const ExprPtr &expr,
                std::vector<std::unordered_map<std::string, TypePtr>> &env,
                Substitution &subst, TypeGenerator &gen);

auto resolve_type(const Substitution &subst, const TypePtr &type) -> TypePtr;

void finalize_types(const ExprPtr &expr, const Substitution &subst);
