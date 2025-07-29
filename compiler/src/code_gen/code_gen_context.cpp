#include "code_gen_context.hpp"

#include "binaryen-c.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

namespace {

auto get_types(
    const std::unordered_map<std::string, BinaryenVariable> &variables)
    -> std::vector<BinaryenType> {
    std::vector<std::pair<std::string, BinaryenVariable>> sorted_variables(
        variables.begin(), variables.end());

    std::ranges::sort(sorted_variables, [](const auto &a, const auto &b) {
        return a.second.local < b.second.local;
    });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::ranges::transform(sorted_variables, std::back_inserter(var_types),
                           [](const auto &p) { return p.second.type; });

    return var_types;
}

} // namespace

auto BinaryenVariable::get_local(BinaryenModuleRef module) const
    -> BinaryenExpressionRef {
    return BinaryenLocalGet(module, local, type);
}

auto BinaryenVariable::set_local(BinaryenModuleRef module,
                                 BinaryenExpressionRef value) const
    -> BinaryenExpressionRef {
    return BinaryenLocalSet(module, local, value);
}

CodeGenContext::CodeGenContext(BinaryenModuleRef module) : module_(module) {}

auto CodeGenContext::module() -> BinaryenModuleRef { return module_; }

auto CodeGenContext::has_constant(const std::string &name) -> bool {
    return constants.contains(name);
}

auto CodeGenContext::constant(const std::string &name)
    -> BinaryenExpressionRef {
    return BinaryenConst(module_, constants.at(name));
}

void CodeGenContext::add_constant(const std::string &name,
                                  BinaryenLiteral expr) {
    constants.emplace(name, expr);
}

auto CodeGenContext::has_variable_or_parameter(const std::string &name)
    -> bool {
    return std::ranges::any_of(
               variables,
               [&](const auto &ctx) { return ctx.contains(name); }) ||
           std::ranges::any_of(
               parameters, [&](const auto &ctx) { return ctx.contains(name); });
}

auto CodeGenContext::variable_or_parameter(const std::string &name)
    -> std::pair<BinaryenVariable &, int> {
    auto depth = 0;
    for (int i = variables.size() - 1; i >= 0; --i) {
        auto it = variables[i].find(name);
        if (it != variables[i].end()) return {it->second, depth};
        it = parameters[i].find(name);
        if (it != parameters[i].end()) return {it->second, depth};
        depth++;
    }
    throw std::runtime_error("Unknown variable: " + name);
}

auto CodeGenContext::add_parameter(const std::string &name, BinaryenType type)
    -> BinaryenVariable & {
    parameters.back().emplace(
        name, BinaryenVariable{
                  .local = static_cast<BinaryenIndex>(parameters.back().size()),
                  .type = type,
                  .offset = offset(),
              });
    offsets.back() += type == BinaryenTypeFloat64() ? 8 : 4;
    return parameters.back().at(name);
}

auto CodeGenContext::add_env() -> BinaryenVariable & {
    return add_parameter("env$", BinaryenTypeInt32());
}

auto CodeGenContext::add_variable(BinaryenType type) -> BinaryenVariable & {
    auto name = "var$" + std::to_string(parameters.back().size() +
                                        variables.back().size());
    return add_variable(name, type);
}

auto CodeGenContext::add_variable(const std::string &name, BinaryenType type)
    -> BinaryenVariable & {
    variables.back().emplace(
        name, BinaryenVariable{
                  .local = static_cast<BinaryenIndex>(parameters.back().size() +
                                                      variables.back().size()),
                  .type = type,
                  .offset = offset(),
              });
    offsets.back() += type == BinaryenTypeFloat64() ? 8 : 4;
    return variables.back().at(name);
}

auto CodeGenContext::has_function(const std::string &name) -> bool {
    return fun_indices.contains(name);
}

auto CodeGenContext::function(const std::string &name) -> BinaryenVariable & {
    return fun_indices.at(name);
}

auto CodeGenContext::add_function(BinaryenExpressionRef body,
                                  BinaryenType result_type,
                                  BinaryenIndex offset) -> BinaryenVariable & {
    auto name = "function$" + std::to_string(fun_indices.size());
    return add_function(name, body, result_type, offset);
}

auto CodeGenContext::add_function(const std::string &name,
                                  BinaryenExpressionRef body,
                                  BinaryenType result_type,
                                  BinaryenIndex offset) -> BinaryenVariable & {
    fun_indices.emplace(
        name, BinaryenVariable{
                  .local = static_cast<BinaryenIndex>(fun_indices.size()),
                  .type = BinaryenTypeInt32(),
                  .offset = offset + 16,
              });

    BinaryenAddFunction(module_, name.c_str(),
                        BinaryenTypeCreate(get_types(parameters.back()).data(),
                                           parameters.back().size()),
                        result_type, get_types(variables.back()).data(),
                        variables.back().size(), body);

    return fun_indices.at(name);
}

auto CodeGenContext::has_buffer(const std::string &name) -> bool {
    return buffers_.contains(name);
}

auto CodeGenContext::buffers()
    -> const std::unordered_map<std::string, BinaryenIndex> & {
    return buffers_;
}

void CodeGenContext::add_buffer(const std::string &name, BinaryenIndex size,
                                BinaryenExpressionRef init_buffer_function) {
    BinaryenAddGlobal(module(), name.c_str(), BinaryenTypeInt32(), true,
                      BinaryenConst(module(), BinaryenLiteralInt32(0)));

    BinaryenAddGlobal(module(), (name + "$size").c_str(), BinaryenTypeInt32(),
                      false,
                      BinaryenConst(module(), BinaryenLiteralInt32(size)));

    BinaryenAddGlobal(module(), (name + "$future").c_str(),
                      BinaryenTypeFloat64(), true,
                      BinaryenConst(module(), BinaryenLiteralFloat64(0.0)));

    auto function_body = std::array{
        env().set_local(module(),
                        BinaryenConst(module(), BinaryenLiteralInt32(1024))),

        BinaryenStore(module(), 4, 0, 4, env().get_local(module()),
                      env().get_local(module()), BinaryenTypeInt32(), "memory"),

        init_buffer_function,
    };

    add_function(name + "$init",
                 BinaryenBlock(module(), nullptr, function_body.data(),
                               function_body.size(), BinaryenTypeFloat64()),
                 BinaryenTypeFloat64(), 0);

    buffers_.emplace(name, (buffers_.size() * 1024 * 8) + 4096);
}

auto CodeGenContext::offset() -> BinaryenIndex { return offsets.back(); }

auto CodeGenContext::env() -> BinaryenVariable & {
    return (variables.back().contains("env$") ? variables : parameters)
        .back()
        .at("env$");
}

auto CodeGenContext::make_closure(const BinaryenVariable &fun)
    -> BinaryenExpressionRef {
    auto env_offset = offset();
    offsets.back() += 4;
    auto fun_offset = offset();
    offsets.back() += 4;

    auto result = std::array{
        BinaryenStore(module(), 4, env_offset, 4, env().get_local(module()),
                      env().get_local(module()), BinaryenTypeInt32(), "memory"),
        BinaryenStore(module(), 4, fun_offset, 4, env().get_local(module()),
                      BinaryenConst(module(), BinaryenLiteralInt32(fun.local)),
                      BinaryenTypeInt32(), "memory"),
        BinaryenBinary(
            module(), BinaryenAddInt32(), env().get_local(module()),
            BinaryenConst(module(), BinaryenLiteralInt32(env_offset))),
    };

    offsets.back() += fun.offset;

    return BinaryenBlock(module(), nullptr, result.data(), result.size(),
                         BinaryenTypeInt32());
}

void CodeGenContext::push_context() {
    parameters.emplace_back();
    variables.emplace_back();
    offsets.emplace_back(8);
}

void CodeGenContext::pop_context() {
    parameters.pop_back();
    variables.pop_back();
    offsets.pop_back();
}

void CodeGenContext::add_function_table() {
    if (!fun_indices.empty()) {
        std::vector<std::pair<std::string, BinaryenVariable>> sorted_funs(
            fun_indices.begin(), fun_indices.end());

        std::ranges::sort(sorted_funs, [](const auto &a, const auto &b) {
            return a.second.local < b.second.local;
        });

        BinaryenAddTable(module(), "fun_table", sorted_funs.size(),
                         sorted_funs.size(), BinaryenTypeFuncref());
        std::vector<std::string> names;
        std::vector<const char *> elems;
        elems.reserve(sorted_funs.size());
        names.reserve(sorted_funs.size());
        for (const auto &[name, pair] : sorted_funs) {
            names.emplace_back(name);
            elems.emplace_back(names.back().c_str());
        }

        BinaryenAddActiveElementSegment(
            module(), "fun_table", "fun_table_seg", elems.data(), elems.size(),
            BinaryenConst(module(), BinaryenLiteralInt32(0)));
    }
}