#pragma once

#include "ast/ast.hpp"
#include "ir.hpp"

#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace lower_detail {

auto ir_type_of(const TypePtr &t) -> IRType;
auto ir_types_of(const TypePtr &t) -> std::vector<IRType>;
auto eval_const_expr(const ExprPtr &e) -> double;

struct FnInfo {
    std::vector<std::string> free_vars;
    std::vector<IRType> return_type;
    size_t arity{0};
    bool is_math{false};
};

struct Lowerer {
    IRModule mod;
    std::vector<IRInstr> *cur = nullptr;

    std::unordered_map<std::string, FnInfo> fns;
    std::unordered_map<std::string, size_t> fn_indices;
    std::unordered_set<std::string> bufs;
    std::unordered_set<std::string> statics;
    std::unordered_set<std::string> param_names;
    std::unordered_map<std::string, IRType> locals;
    std::vector<IRInstr> static_init_body;
    std::unordered_map<std::string, std::vector<std::string>> array_env;
    std::unordered_set<std::string> static_arrays;
    std::unordered_set<std::string> memory_arrays;
    std::unordered_map<std::string, std::string> inline_alias;

    size_t tmp_n = 0;

    auto tmp() -> std::string;
    void emit(IRInstr i) const;
    void define(const std::string &name, IRType type);

    [[nodiscard]] auto is_special(const std::string &name) const -> bool;

    auto read_array_elem(const std::string &arr_name, size_t i) -> IRValue;
    auto emit_global_read(const std::string &name, IRType type) -> IRValue;
    auto lower_tail_as_array(const ExprPtr &e, size_t n)
        -> std::vector<IRValue>;

    void
    lower_fn_body(IRFunction &fn,
                  const std::vector<std::pair<std::string, IRType>> &params,
                  const ExprPtr &body);

    auto free_vars_of(const ExprPtr &e, std::unordered_set<std::string> bound)
        -> std::unordered_set<std::string>;

    void lift(const std::string &name, const ExprPtr &e);

    auto inline_lambda_body(const ExprPtr &lam_expr,
                            const std::vector<std::string> &arg_locals)
        -> std::optional<IRValue>;

    static auto flatten_calls(const ExprPtr &e)
        -> std::pair<const ExprPtr *, std::vector<const ExprPtr *>>;

    void lower_bind_map(const std::string &dest_name, const ExprPtr &value);
    void lower_bind_zip(const std::string &dest_name, const ExprPtr &value);
    auto lower_foldr(const std::vector<const ExprPtr *> &arg_ptrs)
        -> std::optional<IRValue>;

    auto lower_bind(const Bind &node) -> std::optional<IRValue>;
    auto lower_static_bind(const StaticBind &node) -> std::optional<IRValue>;
    auto lower_param_bind(const ParamBind &node) -> std::optional<IRValue>;
    auto lower_call(const ExprPtr &e) -> std::optional<IRValue>;

    auto lower_expr(const ExprPtr &e) -> std::optional<IRValue>;

    void lower_main(const ExprPtr &main_ast);
    void scan_arity(const std::vector<IRInstr> &body);
    void pre_register_math_builtins();
    void pre_register_fns(const ExprPtr &program);
    void compute_arity();
};

} // namespace lower_detail
