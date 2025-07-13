#include "code_gen.hpp"
#include "ast.hpp"
#include "tokenizer.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <array>
#include <cmath>
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

auto CodeGen::create(const ExprPtr &expr) -> BinaryenExpressionRef {
    auto get_literal = [&](const Expr::Literal &lit) {
        try {
            double value = std::stod(std::string(lit.value.lexeme));
            return BinaryenConst(module, BinaryenLiteralFloat64(value));
        } catch (const std::exception &e) {
            throw std::runtime_error("Invalid literal value: " +
                                     std::string(lit.value.lexeme));
        }
    };

    auto get_variable =
        [&](const Expr::Variable &var) -> BinaryenExpressionRef {
        const std::string name = std::string(var.name.lexeme);

        if (auto it = globals.find(name); it != globals.end()) {
            if (name == "TIME")
                return BinaryenBinary(
                    module, BinaryenMulFloat64(),
                    BinaryenGlobalGet(module, "TIME", BinaryenTypeFloat64()),
                    BinaryenConst(module, BinaryenLiteralFloat64(sample_freq)));

            return BinaryenGlobalGet(module, name.c_str(), it->second);
        }

        if (auto it = constants.find(name); it != constants.end())
            return BinaryenConst(module, it->second);

        if (auto it = variables.find(name); it != variables.end())
            return BinaryenLocalGet(module, it->second.first,
                                    it->second.second);

        throw std::runtime_error("Unknown variable: " + name);
    };

    auto get_binary = [&](const Expr::Binary &bin) -> BinaryenExpressionRef {
        auto *lhs = create(bin.lhs);
        auto *rhs = create(bin.rhs);

        switch (bin.op.kind) {
        case TokenKind::Plus:
            return BinaryenBinary(module, BinaryenAddFloat64(), lhs, rhs);
        case TokenKind::Minus:
            return BinaryenBinary(module, BinaryenSubFloat64(), lhs, rhs);
        case TokenKind::Star:
            return BinaryenBinary(module, BinaryenMulFloat64(), lhs, rhs);
        case TokenKind::Slash:
            return BinaryenBinary(module, BinaryenDivFloat64(), lhs, rhs);
        default:
            throw std::runtime_error("Unsupported binary operator");
        }
    };

    auto get_assignment =
        [&](const Expr::Assignment &asg) -> BinaryenExpressionRef {
        auto *val = create(asg.value);
        const std::string name = std::string(asg.name.lexeme);

        auto it = variables.find(name);
        if (it == variables.end()) {
            BinaryenIndex index = parameters.size() + variables.size();
            variables.emplace(name,
                              std::make_pair(index, BinaryenTypeFloat64()));
            return BinaryenLocalSet(module, index, val);
        }
        return BinaryenLocalSet(module, it->second.first, val);
    };

    auto get_call = [&](const Expr::Call &call) -> BinaryenExpressionRef {
        if (const auto *callee_var =
                std::get_if<Expr::Variable>(&call.callee->node)) {
            const std::string name = std::string(callee_var->name.lexeme);

            if (name == "sin") {
                BinaryenExpressionRef arg = create(call.argument);
                return BinaryenCall(module, "wasmwasm_sin", &arg, 1,
                                    BinaryenTypeFloat64());
            }

            if (function_indices.contains(name)) {
                BinaryenExpressionRef arg = create(call.argument);
                return BinaryenCall(module, name.c_str(), &arg, 1,
                                    BinaryenTypeFloat64());
            }

            throw std::runtime_error("Unknown function: " + name);
        }

        BinaryenExpressionRef func_idx = create(call.callee);
        std::array<BinaryenExpressionRef, 1> args = {create(call.argument)};
        return BinaryenCallIndirect(
            module, "func_table", func_idx, args.data(), 1,
            BinaryenTypeCreate(
                std::array<BinaryenType, 1>{BinaryenTypeFloat64()}.data(), 1),
            BinaryenTypeFloat64());
    };

    auto get_block = [&](const Expr::Block &block) -> BinaryenExpressionRef {
        std::vector<BinaryenExpressionRef> children;
        children.reserve(block.size());
        for (const auto &expr : block)
            children.emplace_back(create(expr));

        return BinaryenBlock(module, nullptr, children.data(), children.size(),
                             BinaryenTypeNone());
    };

    auto get_lambda = [&](const Expr::Lambda &lambda) -> BinaryenExpressionRef {
        auto free_vars = collect_free_vars(lambda.body, {});
        for (const auto &v : free_vars)
            if (!std::ranges::any_of(lambda.parameters, [&](const auto &param) {
                    return param.name.lexeme == v;
                }))
                throw std::runtime_error("Lambda captures variable: " + v);

        const std::string name =
            "lambda$" + std::to_string(lambda_functions.size());

        auto old_variables = std::exchange(variables, {});
        for (size_t i = 0; i < lambda.parameters.size(); ++i)
            variables[lambda.parameters[i].name.lexeme] = {
                i, BinaryenTypeFloat64()};

        BinaryenExpressionRef body = create(lambda.body);

        BinaryenFunctionRef func =
            BinaryenAddFunction(module, name.c_str(),
                                BinaryenTypeCreate(get_types(variables).data(),
                                                   lambda.parameters.size()),
                                BinaryenTypeFloat64(), nullptr, 0, body);

        variables = std::move(old_variables);

        function_table.push_back(name);
        lambda_functions.emplace_back(name, func);
        function_indices[name] = function_table.size() - 1;

        return BinaryenConst(module,
                             BinaryenLiteralInt32(function_indices[name]));
    };

    auto visitor = overloaded{
        [&](const Expr::Literal &lit) { return get_literal(lit); },
        [&](const Expr::Variable &var) { return get_variable(var); },
        [&](const Expr::Binary &bin) { return get_binary(bin); },
        [&](const Expr::Assignment &asg) { return get_assignment(asg); },
        [&](const Expr::Call &call) { return get_call(call); },
        [&](const Expr::Block &block) { return get_block(block); },
        [&](const Expr::Lambda &lambda) { return get_lambda(lambda); }};

    return std::visit(visitor, expr->node);
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
            for (const auto &e : b)
                visit(e);
        }

        void visit_node(const Expr::Lambda &l) {
            Set inner_bound = bound;
            for (const auto &param : l.parameters)
                inner_bound.insert(std::string(param.name.lexeme));

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
    move_module_items(math_module->memories,
                      [&](auto m) { module->addMemory(std::move(m)); });
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

    BinaryenExpressionRef body =
        BinaryenBlock(module, "body", body_block.data(), body_block.size(),
                      BinaryenTypeNone());

    BinaryenAddFunction(
        module, "main",
        BinaryenTypeCreate(get_types(parameters).data(), parameters.size()),
        BinaryenTypeNone(), get_types(variables).data(), variables.size(),
        body);
    BinaryenAddFunctionExport(module, "main", "main");

    if (!function_table.empty())
        BinaryenAddTable(module, "func_table", 0, function_table.size(),
                         BinaryenTypeFuncref());

    module->features = BinaryenFeatureAll();
    return module;
}