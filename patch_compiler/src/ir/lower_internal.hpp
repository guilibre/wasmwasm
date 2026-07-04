#pragma once

#include "ast/ast.hpp"
#include "ir.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace lower_detail {

inline constexpr size_t closure_max_captures = 8;

inline auto closure_slot_names(const std::string &param_name)
    -> std::vector<std::string> {
    std::vector<std::string> slots;
    slots.reserve(closure_max_captures + 1);
    slots.push_back(param_name + "$idx");
    for (size_t c = 0; c < closure_max_captures; ++c)
        slots.push_back(param_name + "$c" + std::to_string(c));
    return slots;
}

auto ir_type_of(const TypePtr &t) -> IRType;
auto ir_types_of(const TypePtr &t) -> std::vector<IRType>;
auto eval_const_expr(const ExprPtr &e) -> double;

inline constexpr size_t kMaxUnrollIterations = size_t{1} << 20;

enum class ParamKind : uint8_t { Scalar, Array, Closure };

struct ParamShape {
    ParamKind kind{ParamKind::Scalar};
    size_t size{0};
};

struct FnInfo {
    std::vector<std::string> free_vars;
    std::vector<IRType> return_type;
    size_t arity{0};
    bool is_math{false};
    std::vector<ParamShape> param_shapes;
};

struct UnrollShape {
    size_t counter_param_idx{0};
    double decrement{0.0};
    Operation cmp_op{Operation::Lt};
    double threshold{0.0};
    bool recursive_on_true{false};
    const ExprPtr *base_branch{nullptr};
    const ExprPtr *recursive_branch{nullptr};
    std::vector<const ExprPtr *> recursive_call_args;
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
    std::unordered_set<std::string> closure_params;
    std::unordered_map<std::string, std::vector<ParamShape>> call_site_shapes;
    std::unordered_map<const Expr *, std::string> lambda_arg_names;
    std::unordered_set<std::string> lowered_fns;
    std::unordered_map<std::string, size_t> known_arities;
    std::string cur_fn_name = "main$body";
    std::unordered_map<std::string, const ExprPtr *> fn_ast;
    std::unordered_map<std::string, std::optional<UnrollShape>>
        unroll_shape_cache;

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

    void register_fn_signature(const std::string &name, const ExprPtr &e);
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
    auto lower_closure_arg(const ExprPtr &ap) -> std::vector<IRValue>;

    auto try_unroll_bounded_recursion(const ExprPtr &call_expr,
                                      std::optional<IRValue> &out) -> bool;
    auto classify_unrollable(const std::string &fn_name,
                             const ExprPtr *root_lambda)
        -> std::optional<UnrollShape>;
    auto unroll_call(const std::string &fn_name, const UnrollShape &shape,
                     double counter_value,
                     const std::vector<std::string> &arg_locals)
        -> std::optional<IRValue>;

    auto lower_expr(const ExprPtr &e) -> std::optional<IRValue>;

    void lower_main(const ExprPtr &main_ast);
    void scan_arity(const std::vector<IRInstr> &body);
    void pre_register_math_builtins();
    void pre_register_fns(const ExprPtr &program);
    void pre_register_arities(const ExprPtr &program);
    [[nodiscard]] auto shape_of_call_arg(const ExprPtr &arg) const
        -> ParamShape;
    void scan_call_site_shapes(const ExprPtr &e);
    void pre_register_lambda_args(const ExprPtr &e);
    void compute_arity();
};

} // namespace lower_detail
