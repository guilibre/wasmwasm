#include "code_gen.hpp"

#include "../ast/ast.hpp"
#include "../parser/tokenizer.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <iterator>
#include <memory>
#include <numbers>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace {

template <class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

auto get_types(const std::unordered_map<
               std::string, std::pair<BinaryenIndex, BinaryenType>> &variables)
    -> std::vector<BinaryenType> {
    std::vector<std::pair<std::string, std::pair<BinaryenIndex, BinaryenType>>>
        sorted_variables(variables.begin(), variables.end());

    std::ranges::sort(sorted_variables, [](const auto &a, const auto &b) {
        return a.second.first < b.second.first;
    });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::ranges::transform(sorted_variables, std::back_inserter(var_types),
                           [](const auto &p) { return p.second.second; });

    return var_types;
}

auto map_to_binaryen_type(const TypePtr &type) -> BinaryenType {
    if (auto *base = std::get_if<TypeBase>(&type->node)) {
        switch (base->kind) {
        case BaseTypeKind::Float:
            return BinaryenTypeFloat64();
        case BaseTypeKind::Int:
        case BaseTypeKind::Bool:
            return BinaryenTypeInt32();
        }
    }

    if (std::holds_alternative<TypeFun>(type->node)) return BinaryenTypeInt32();

    return BinaryenTypeInt32();
}

auto get_function_param_types(const TypePtr &type)
    -> std::vector<BinaryenType> {
    std::vector<BinaryenType> params;
    params.push_back(BinaryenTypeInt32());

    TypePtr current = type;
    while (auto *fun = std::get_if<TypeFun>(&current->node)) {
        params.push_back(map_to_binaryen_type(fun->param));
        current = fun->result;
    }

    return params;
}

} // namespace

CodeGen::CodeGen(BinaryenModuleRef math_module, double sample_freq)
    : module(BinaryenModuleCreate()), math_module(math_module),
      sample_freq(sample_freq) {
    if (module == nullptr)
        throw std::runtime_error("Failed to create Binaryen module");
}

CodeGen::~CodeGen() {
    if (module != nullptr) BinaryenModuleDispose(module);
}

auto CodeGen::make_closure(
    BinaryenExpressionRef func_index_expr,
    const std::vector<BinaryenExpressionRef> &captured_values)
    -> BinaryenExpressionRef {
    int32_t env_size = captured_values.size() * 8;
    int32_t env_ptr_val = heap_top;
    heap_top += env_size;

    auto *env_ptr = BinaryenConst(module, BinaryenLiteralInt32(env_ptr_val));

    std::vector<BinaryenExpressionRef> stores;
    stores.reserve(captured_values.size() + 3);

    for (size_t i = 0; i < captured_values.size(); ++i) {
        auto *offset = BinaryenConst(module, BinaryenLiteralInt32(i * 8));
        auto *addr =
            BinaryenBinary(module, BinaryenAddInt32(), env_ptr, offset);

        stores.push_back(BinaryenStore(module, 8, 0, 8, addr,
                                       captured_values[i],
                                       BinaryenTypeFloat64(), "memory"));
    }

    int32_t closure_ptr_val = heap_top;
    heap_top += 8;
    auto *closure_ptr =
        BinaryenConst(module, BinaryenLiteralInt32(closure_ptr_val));

    stores.push_back(BinaryenStore(module, 4, 0, 4, closure_ptr,
                                   func_index_expr, BinaryenTypeInt32(),
                                   "memory"));

    stores.push_back(BinaryenStore(module, 4, 4, 4, closure_ptr, env_ptr,
                                   BinaryenTypeInt32(), "memory"));

    stores.push_back(closure_ptr);

    return BinaryenBlock(module, nullptr, stores.data(), stores.size(),
                         BinaryenTypeInt32());
}

auto CodeGen::create(const ExprPtr &expr) -> BinaryenExpressionRef {
    return std::visit(
        [&](const auto &node) -> BinaryenExpressionRef {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Expr::Assignment>) {
                std::string name(node.name.lexeme);
                if (variables.contains(name))
                    throw std::runtime_error("Variable already assigned: " +
                                             name);

                auto *val = create(node.value);

                BinaryenIndex idx = parameters.size() + variables.size();
                variables.emplace(
                    name, std::make_pair(
                              idx, map_to_binaryen_type(node.value->type)));

                return BinaryenLocalSet(module, idx, val);
            }

            if constexpr (std::is_same_v<T, Expr::Binary>) {
                auto *lhs = create(node.lhs);
                auto *rhs = create(node.rhs);

                switch (node.op.kind) {
                case TokenKind::Plus:
                    return BinaryenBinary(module, BinaryenAddFloat64(), lhs,
                                          rhs);
                case TokenKind::Minus:
                    return BinaryenBinary(module, BinaryenSubFloat64(), lhs,
                                          rhs);
                case TokenKind::Star:
                    return BinaryenBinary(module, BinaryenMulFloat64(), lhs,
                                          rhs);
                case TokenKind::Slash:
                    return BinaryenBinary(module, BinaryenDivFloat64(), lhs,
                                          rhs);
                default:
                    throw std::runtime_error("Unsupported binary operator");
                }
            }

            if constexpr (std::is_same_v<T, Expr::Block>) {
                std::vector<BinaryenExpressionRef> children;
                children.reserve(node.expressions.size());
                for (const auto &e : node.expressions)
                    children.emplace_back(create(e));

                BinaryenType block_type = BinaryenTypeNone();
                if (!node.expressions.empty() &&
                    !std::holds_alternative<Expr::Assignment>(
                        node.expressions.back()->node))
                    block_type = BinaryenTypeFloat64();

                return BinaryenBlock(module, nullptr, children.data(),
                                     children.size(), block_type);
            }

            if constexpr (std::is_same_v<T, Expr::Call>) {
                const auto &call = node;

                auto callee_expr = create(call.callee);
                auto arg_expr = create(call.argument);

                if (auto *var =
                        std::get_if<Expr::Variable>(&call.callee->node)) {
                    std::string name(var->name.lexeme);

                    if (name == "sin")
                        return BinaryenCall(module, "wasmwasm_sin", &arg_expr,
                                            1, BinaryenTypeFloat64());

                    if (function_indices.contains(name))
                        return BinaryenCall(module, name.c_str(), &arg_expr, 1,
                                            BinaryenTypeFloat64());

                    throw std::runtime_error("Unknown direct function: " +
                                             name);
                }

                auto func_index =
                    BinaryenLoad(module, 4, false, 0, 4, BinaryenTypeInt32(),
                                 callee_expr, "memory");
                auto env_ptr =
                    BinaryenLoad(module, 4, false, 4, 4, BinaryenTypeInt32(),
                                 callee_expr, "memory");

                std::array<BinaryenExpressionRef, 2> call_args = {env_ptr,
                                                                  arg_expr};
                auto param_types = get_function_param_types(expr->type);
                auto result_type = map_to_binaryen_type(
                    std::get<TypeFun>(expr->type->node).result);

                return BinaryenCallIndirect(
                    module, "func_table", func_index, call_args.data(),
                    call_args.size(),
                    BinaryenTypeCreate(param_types.data(), param_types.size()),
                    result_type);
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                auto free_vars = collect_free_vars(
                    expr, {std::string(node.parameter.lexeme)});

                std::unordered_map<std::string, int32_t> env_offsets;
                int32_t offset = 0;
                for (const auto &v : free_vars)
                    env_offsets[v] = offset++;

                std::string func_name =
                    "lambda$" + std::to_string(function_indices.size());
                int32_t func_index = function_indices.size();
                function_indices[func_name] = {func_index, nullptr};

                auto old_vars = std::exchange(
                    variables,
                    std::unordered_map<
                        std::string, std::pair<BinaryenIndex, BinaryenType>>{});
                auto old_mem_loaded = std::exchange(
                    mem_loaded_variables, std::unordered_set<std::string>{});

                variables[node.parameter.lexeme] = {1, BinaryenTypeFloat64()};

                for (const auto &[var, idx] : env_offsets) {
                    variables[var] = {idx * 8, BinaryenTypeFloat64()};
                    mem_loaded_variables.insert(var);
                }

                auto *body_expr = create(node.body);

                std::array<BinaryenType, 2> param_types = {
                    BinaryenTypeInt32(), BinaryenTypeFloat64()};
                auto param_type =
                    BinaryenTypeCreate(param_types.data(), param_types.size());

                BinaryenFunctionRef func = BinaryenAddFunction(
                    module, func_name.c_str(), param_type,
                    BinaryenTypeFloat64(), nullptr, 0, body_expr);

                function_indices[func_name] = {func_index, func};

                variables = std::move(old_vars);
                mem_loaded_variables = std::move(old_mem_loaded);

                std::vector<BinaryenExpressionRef> captured_values;
                captured_values.reserve(free_vars.size());
                for (const auto &v : free_vars)
                    captured_values.push_back(
                        create(Expr::make<Expr::Variable>(Token{.lexeme = v})));

                return make_closure(
                    BinaryenConst(module, BinaryenLiteralInt32(func_index)),
                    captured_values);
            }

            if constexpr (std::is_same_v<T, Expr::Literal>) {
                double value = std::stod(std::string(node.value.lexeme));
                return BinaryenConst(module, BinaryenLiteralFloat64(value));
            }

            if constexpr (std::is_same_v<T, Expr::Variable>) {
                std::string name(node.name.lexeme);

                if (name == "PI")
                    return BinaryenConst(
                        module, BinaryenLiteralFloat64(std::numbers::pi));

                if (name == "TIME")
                    return BinaryenGlobalGet(module, name.c_str(),
                                             BinaryenTypeFloat64());

                if (auto it = variables.find(name); it != variables.end()) {
                    return BinaryenLocalGet(module, it->second.first,
                                            it->second.second);
                }
                throw std::runtime_error("Unknown variable: " + name);
            }

            throw std::runtime_error("Unknown expression type in create");
        },
        expr->node);
}

auto CodeGen::collect_free_vars(const ExprPtr &expr,
                                const std::unordered_set<std::string> &bound)
    -> std::unordered_set<std::string> {
    using Set = std::unordered_set<std::string>;

    struct Visitor {
        Set bound;
        Set result;

        void visit(const ExprPtr &e) { // NOLINT
            std::visit([&](const auto &node) { visit_node(node); }, e->node);
        }

        void visit_node(const Expr::Literal & /*unused*/) {}

        void visit_node(const Expr::Variable &v) {
            std::string name = std::string(v.name.lexeme);
            if (!bound.contains(name)) result.insert(name);
        }

        void visit_node(const Expr::Binary &b) {
            visit(b.lhs);
            visit(b.rhs);
        }

        void visit_node(const Expr::Assignment &a) { visit(a.value); }

        void visit_node(const Expr::Call &c) {
            visit(c.callee);
            if (c.argument) visit(c.argument);
        }

        void visit_node(const Expr::Block &b) {
            for (const auto &e : b.expressions)
                visit(e);
        }

        void visit_node(const Expr::Lambda &l) {
            Set inner_bound = bound;
            inner_bound.insert(std::string(l.parameter.lexeme));

            auto inner_free = collect_free_vars(l.body, inner_bound);
            result.insert(inner_free.begin(), inner_free.end());
        }
    };

    Visitor v{.bound = bound};
    v.visit(expr);
    return v.result;
}

auto CodeGen::create_main_module(const std::vector<ExprPtr> &exprs)
    -> BinaryenModuleRef {
    if (module == nullptr) throw std::runtime_error("Module not initialized");

    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);

    auto move_module_items = [&](auto &from, auto add_fn) {
        while (!from.empty()) {
            auto item = std::move(from.back());
            from.pop_back();
            add_fn(std::move(item));
        }
    };

    move_module_items(math_module->globals,
                      [&](auto g) { module->addGlobal(std::move(g)); });
    move_module_items(math_module->functions, [&](auto f) {
        for (const auto &ex : math_module->exports) {
            if (ex->getInternalName()->str == f->name.str) {
                f->setExplicitName(ex->name);
                break;
            }
        }
        module->addFunction(std::move(f));
    });

    auto make_const_i32 = [&](int32_t val) {
        return BinaryenConst(module, BinaryenLiteralInt32(val));
    };
    auto make_const_f64 = [&](double val) {
        return BinaryenConst(module, BinaryenLiteralFloat64(val));
    };

    auto time_type = BinaryenTypeFloat64();
    BinaryenAddGlobal(module, "TIME", time_type, true, make_const_f64(0.0));
    globals.emplace("TIME", time_type);
    auto time_get = [&]() {
        return BinaryenGlobalGet(module, "TIME", time_type);
    };

    constants.emplace("PI", BinaryenLiteralFloat64(std::numbers::pi_v<double>));

    auto add_param = [&](const std::string &name, BinaryenType type) {
        parameters.emplace(name, std::make_pair(parameters.size(), type));
    };
    add_param("BASE_PTR", BinaryenTypeInt32());
    add_param("NUM_SAMPLES", BinaryenTypeInt32());
    add_param("NUM_CHANNELS", BinaryenTypeInt32());

    auto get_param = [&](const std::string &name) {
        auto [idx, type] = parameters.at(name);
        return BinaryenLocalGet(module, idx, type);
    };

    auto add_var = [&](const std::string &name, BinaryenType type) {
        variables.emplace(
            name, std::make_pair(variables.size() + parameters.size(), type));
    };
    add_var("CHANNEL", BinaryenTypeInt32());
    add_var("SAMPLE", BinaryenTypeInt32());

    auto get_var = [&](const std::string &name) {
        auto [idx, type] = variables.at(name);
        return BinaryenLocalGet(module, idx, type);
    };
    auto set_var = [&](const std::string &name, BinaryenExpressionRef expr) {
        auto [idx, _] = variables.at(name);
        return BinaryenLocalSet(module, idx, expr);
    };

    auto *sample_cond =
        BinaryenBinary(module, BinaryenLtUInt32(), get_var("SAMPLE"),
                       get_param("NUM_SAMPLES"));
    auto *channel_cond =
        BinaryenBinary(module, BinaryenLtUInt32(), get_var("CHANNEL"),
                       get_param("NUM_CHANNELS"));

    auto *index_expr = BinaryenBinary(
        module, BinaryenAddInt32(), get_var("CHANNEL"),
        BinaryenBinary(module, BinaryenMulInt32(), get_var("SAMPLE"),
                       get_param("NUM_CHANNELS")));

    auto *address =
        BinaryenBinary(module, BinaryenAddInt32(), get_param("BASE_PTR"),
                       BinaryenBinary(module, BinaryenMulInt32(),
                                      make_const_i32(4), index_expr));

    std::vector<BinaryenExpressionRef> binaryen_exprs;
    binaryen_exprs.reserve(exprs.size());
    for (const auto &expr : exprs)
        binaryen_exprs.emplace_back(create(expr));

    auto *output_block =
        BinaryenBlock(module, "output_block", binaryen_exprs.data(),
                      binaryen_exprs.size(), BinaryenTypeNone());

    if (!variables.contains("OUT"))
        throw std::runtime_error("Output variable OUT not defined");

    auto [idx, type] = variables.at("OUT");
    auto *get_out = BinaryenLocalGet(module, idx, type);

    auto *assign_out =
        BinaryenStore(module, 4, 0, 4, address,
                      BinaryenUnary(module, BinaryenDemoteFloat64(), get_out),
                      BinaryenTypeFloat32(), "memory");

    std::vector<BinaryenExpressionRef> inner_block_children = {
        output_block, assign_out,
        set_var("CHANNEL",
                BinaryenBinary(module, BinaryenAddInt32(), get_var("CHANNEL"),
                               make_const_i32(1))),
        BinaryenBreak(module, "inner_loop", channel_cond, nullptr)};
    auto *inner_loop = BinaryenLoop(
        module, "inner_loop",
        BinaryenBlock(module, "inner_block", inner_block_children.data(),
                      inner_block_children.size(), BinaryenTypeNone()));

    auto *pass_time =
        BinaryenGlobalSet(module, "TIME",
                          BinaryenBinary(module, BinaryenAddFloat64(),
                                         time_get(), make_const_f64(1.0)));

    std::vector<BinaryenExpressionRef> outer_block_children = {
        set_var("CHANNEL", make_const_i32(0)), inner_loop, pass_time,
        set_var("SAMPLE", BinaryenBinary(module, BinaryenAddInt32(),
                                         get_var("SAMPLE"), make_const_i32(1))),
        BinaryenBreak(module, "outer_loop", sample_cond, nullptr)};
    auto *outer_loop = BinaryenLoop(
        module, "outer_loop",
        BinaryenBlock(module, "outer_block", outer_block_children.data(),
                      outer_block_children.size(), BinaryenTypeNone()));

    std::vector<BinaryenExpressionRef> body_block = {
        set_var("SAMPLE", make_const_i32(0)), outer_loop};

    auto *body = BinaryenBlock(module, "body", body_block.data(),
                               body_block.size(), BinaryenTypeNone());

    BinaryenAddFunction(
        module, "main",
        BinaryenTypeCreate(get_types(parameters).data(), parameters.size()),
        BinaryenTypeNone(), get_types(variables).data(), variables.size(),
        body);
    BinaryenAddFunctionExport(module, "main", "main");

    if (!function_indices.empty()) {
        BinaryenAddTable(module, "func_table", function_indices.size(),
                         function_indices.size(), BinaryenTypeFuncref());
        std::vector<std::string> names;
        std::vector<const char *> elems;
        elems.reserve(function_indices.size());
        names.reserve(function_indices.size());
        for (const auto &[name, pair] : function_indices) {
            names.emplace_back(name);
            elems.emplace_back(names.back().c_str());
        }

        BinaryenAddActiveElementSegment(
            module, "func_table", "func_table_seg", elems.data(), elems.size(),
            BinaryenConst(module, BinaryenLiteralInt32(0)));
    }

    module->features = BinaryenFeatureAll();
    return module;
}