#include "binaryen_simd_vectorize_pass.hpp"

#include <array>
#include <cstdint>
#include <optional>
#include <string_view>
#include <vector>

namespace {

auto as_offset_ptr(BinaryenExpressionRef ptr, int32_t &offset_out)
    -> BinaryenExpressionRef {
    if (BinaryenExpressionGetId(ptr) != BinaryenBinaryId()) return nullptr;
    if (BinaryenBinaryGetOp(ptr) != BinaryenAddInt32()) return nullptr;

    auto *rhs = BinaryenBinaryGetRight(ptr);
    if (BinaryenExpressionGetId(rhs) != BinaryenConstId()) return nullptr;
    if (BinaryenExpressionGetType(rhs) != BinaryenTypeInt32()) return nullptr;

    auto *lhs = BinaryenBinaryGetLeft(ptr);
    if (BinaryenExpressionGetId(lhs) == BinaryenConstId()) return nullptr;

    offset_out = BinaryenConstGetValueI32(rhs);
    return ptr;
}

auto expressions_structurally_equal(BinaryenExpressionRef a,
                                    BinaryenExpressionRef b) -> bool {
    if (a == b) return true;
    auto id = BinaryenExpressionGetId(a);
    if (id != BinaryenExpressionGetId(b)) return false;
    if (BinaryenExpressionGetType(a) != BinaryenExpressionGetType(b))
        return false;

    if (id == BinaryenLocalGetId())
        return BinaryenLocalGetGetIndex(a) == BinaryenLocalGetGetIndex(b);
    if (id == BinaryenConstId()) {
        auto type = BinaryenExpressionGetType(a);
        if (type == BinaryenTypeInt32())
            return BinaryenConstGetValueI32(a) == BinaryenConstGetValueI32(b);
        if (type == BinaryenTypeFloat64())
            return BinaryenConstGetValueF64(a) == BinaryenConstGetValueF64(b);
        return false;
    }
    if (id == BinaryenBinaryId()) {
        if (BinaryenBinaryGetOp(a) != BinaryenBinaryGetOp(b)) return false;
        return expressions_structurally_equal(BinaryenBinaryGetLeft(a),
                                              BinaryenBinaryGetLeft(b)) &&
               expressions_structurally_equal(BinaryenBinaryGetRight(a),
                                              BinaryenBinaryGetRight(b));
    }
    return false;
}

auto f64_binary_to_vec_op(BinaryenOp op, bool &ok) -> BinaryenOp {
    ok = true;
    if (op == BinaryenAddFloat64()) return BinaryenAddVecF64x2();
    if (op == BinaryenSubFloat64()) return BinaryenSubVecF64x2();
    if (op == BinaryenMulFloat64()) return BinaryenMulVecF64x2();
    if (op == BinaryenDivFloat64()) return BinaryenDivVecF64x2();
    if (op == BinaryenMinFloat64()) return BinaryenMinVecF64x2();
    if (op == BinaryenMaxFloat64()) return BinaryenMaxVecF64x2();
    ok = false;
    return op;
}

struct MathCallVecInfo {
    std::string_view scalar_name;
    const char *vec_name;
    BinaryenIndex arity;
};

constexpr std::array<MathCallVecInfo, 7> kVectorizableMathCalls{{
    {
        .scalar_name = "wasmwasm_sin",
        .vec_name = "wasmwasm_sin_x2",
        .arity = 1,
    },
    {
        .scalar_name = "wasmwasm_cos",
        .vec_name = "wasmwasm_cos_x2",
        .arity = 1,
    },
    {
        .scalar_name = "wasmwasm_clip",
        .vec_name = "wasmwasm_clip_x2",
        .arity = 1,
    },
    {
        .scalar_name = "wasmwasm_min",
        .vec_name = "wasmwasm_min_x2",
        .arity = 2,
    },
    {
        .scalar_name = "wasmwasm_max",
        .vec_name = "wasmwasm_max_x2",
        .arity = 2,
    },
    {
        .scalar_name = "wasmwasm_uniform",
        .vec_name = "wasmwasm_uniform_x2",
        .arity = 2,
    },
    {
        .scalar_name = "wasmwasm_gaussian",
        .vec_name = "wasmwasm_gaussian_x2",
        .arity = 2,
    },
}};

auto lookup_vectorizable_math_call(std::string_view name)
    -> const MathCallVecInfo * {
    for (const auto &info : kVectorizableMathCalls)
        if (info.scalar_name == name) return &info;
    return nullptr;
}

struct VectorizeContext {
    BinaryenModuleRef mod;
    BinaryenFunctionRef fn;
};

auto make_temp_v128(const VectorizeContext &ctx, BinaryenExpressionRef value,
                    std::vector<BinaryenExpressionRef> &side_effects)
    -> BinaryenIndex {
    auto tmp_index = BinaryenFunctionAddVar(ctx.fn, BinaryenTypeVec128());
    side_effects.push_back(BinaryenLocalSet(ctx.mod, tmp_index, value));
    return tmp_index;
}

auto get_temp_v128(const VectorizeContext &ctx, BinaryenIndex index)
    -> BinaryenExpressionRef {
    return BinaryenLocalGet(ctx.mod, index, BinaryenTypeVec128());
}

auto try_vectorize_expr_pair(const VectorizeContext &ctx,
                             BinaryenExpressionRef expr_a,
                             BinaryenExpressionRef expr_b,
                             std::vector<BinaryenExpressionRef> &side_effects,
                             bool is_nested_operand = false)
    -> std::optional<BinaryenIndex> {
    if (BinaryenExpressionGetType(expr_a) != BinaryenTypeFloat64() ||
        BinaryenExpressionGetType(expr_b) != BinaryenTypeFloat64())
        return std::nullopt;

    auto id_a = BinaryenExpressionGetId(expr_a);
    auto id_b = BinaryenExpressionGetId(expr_b);

    if (id_a == BinaryenLoadId() && id_b == BinaryenLoadId()) {
        if (BinaryenLoadIsAtomic(expr_a) || BinaryenLoadIsAtomic(expr_b))
            return std::nullopt;
        if (BinaryenLoadGetBytes(expr_a) != 8 ||
            BinaryenLoadGetBytes(expr_b) != 8)
            return std::nullopt;
        if (BinaryenLoadGetOffset(expr_a) != 0 ||
            BinaryenLoadGetOffset(expr_b) != 0)
            return std::nullopt;

        int32_t offset_a = 0;
        int32_t offset_b = 0;
        auto *ptr_a = as_offset_ptr(BinaryenLoadGetPtr(expr_a), offset_a);
        auto *ptr_b = as_offset_ptr(BinaryenLoadGetPtr(expr_b), offset_b);
        if (ptr_a == nullptr || ptr_b == nullptr) return std::nullopt;
        if (offset_b != offset_a + 8) return std::nullopt;

        auto *base_a = BinaryenBinaryGetLeft(ptr_a);
        auto *base_b = BinaryenBinaryGetLeft(ptr_b);
        if (!expressions_structurally_equal(base_a, base_b))
            return std::nullopt;

        auto *vec_load = BinaryenLoad(ctx.mod, 16, false, 0, 8,
                                      BinaryenTypeVec128(), ptr_a, "0");
        return make_temp_v128(ctx, vec_load, side_effects);
    }

    if (id_a == BinaryenBinaryId() && id_b == BinaryenBinaryId()) {
        auto op_a = BinaryenBinaryGetOp(expr_a);
        auto op_b = BinaryenBinaryGetOp(expr_b);
        if (op_a != op_b) return std::nullopt;

        auto op_ok = false;
        auto vec_op = f64_binary_to_vec_op(op_a, op_ok);
        if (!op_ok) return std::nullopt;

        auto left_index = try_vectorize_expr_pair(
            ctx, BinaryenBinaryGetLeft(expr_a), BinaryenBinaryGetLeft(expr_b),
            side_effects, /*is_nested_operand=*/true);
        if (!left_index.has_value()) return std::nullopt;
        const auto right_index = try_vectorize_expr_pair(
            ctx, BinaryenBinaryGetRight(expr_a), BinaryenBinaryGetRight(expr_b),
            side_effects, /*is_nested_operand=*/true);
        if (!right_index.has_value()) return std::nullopt;

        auto *vec_binary =
            BinaryenBinary(ctx.mod, vec_op, get_temp_v128(ctx, *left_index),
                           get_temp_v128(ctx, *right_index));
        return make_temp_v128(ctx, vec_binary, side_effects);
    }

    if (is_nested_operand && id_a == id_b &&
        (id_a == BinaryenConstId() || id_a == BinaryenLocalGetId()) &&
        expressions_structurally_equal(expr_a, expr_b)) {
        auto *splat = BinaryenUnary(ctx.mod, BinaryenSplatVecF64x2(), expr_a);
        return make_temp_v128(ctx, splat, side_effects);
    }

    if (id_a == BinaryenCallId() && id_b == BinaryenCallId()) {
        if (BinaryenCallIsReturn(expr_a) || BinaryenCallIsReturn(expr_b))
            return std::nullopt;

        const std::string_view target_a{BinaryenCallGetTarget(expr_a)};
        const std::string_view target_b{BinaryenCallGetTarget(expr_b)};
        if (target_a != target_b) return std::nullopt;

        const auto *info = lookup_vectorizable_math_call(target_a);
        if (info == nullptr) return std::nullopt;

        if (BinaryenCallGetNumOperands(expr_a) != info->arity ||
            BinaryenCallGetNumOperands(expr_b) != info->arity)
            return std::nullopt;

        std::array<BinaryenIndex, 2> arg_vec_indices{};
        for (BinaryenIndex arg_i = 0; arg_i < info->arity; ++arg_i) {
            auto arg_index = try_vectorize_expr_pair(
                ctx, BinaryenCallGetOperandAt(expr_a, arg_i),
                BinaryenCallGetOperandAt(expr_b, arg_i), side_effects);
            if (!arg_index.has_value()) return std::nullopt;
            arg_vec_indices[arg_i] = *arg_index;
        }

        std::array<BinaryenExpressionRef, 2> call_args{};
        for (BinaryenIndex arg_i = 0; arg_i < info->arity; ++arg_i)
            call_args[arg_i] = get_temp_v128(ctx, arg_vec_indices[arg_i]);

        auto *vec_call = BinaryenCall(ctx.mod, info->vec_name, call_args.data(),
                                      info->arity, BinaryenTypeVec128());
        return make_temp_v128(ctx, vec_call, side_effects);
    }

    return std::nullopt;
}

struct CandidateExpr {
    BinaryenExpressionRef value = nullptr;
    bool is_local_set = false;
    BinaryenIndex local_index = 0;
};

auto match_candidate_expr(BinaryenExpressionRef stmt) -> CandidateExpr {
    CandidateExpr result;
    const auto id = BinaryenExpressionGetId(stmt);
    if (id == BinaryenLoadId() || id == BinaryenBinaryId() ||
        id == BinaryenCallId()) {
        result.value = stmt;
        return result;
    }
    if (id == BinaryenLocalSetId() && !BinaryenLocalSetIsTee(stmt)) {
        result.value = BinaryenLocalSetGetValue(stmt);
        result.is_local_set = true;
        result.local_index = BinaryenLocalSetGetIndex(stmt);
    }
    return result;
}

auto stores_are_vectorizable_pair(BinaryenExpressionRef store_a,
                                  BinaryenExpressionRef store_b,
                                  BinaryenExpressionRef &base_out,
                                  int32_t &offset_out) -> bool {
    if (BinaryenStoreIsAtomic(store_a) || BinaryenStoreIsAtomic(store_b))
        return false;
    if (BinaryenStoreGetBytes(store_a) != 8 ||
        BinaryenStoreGetBytes(store_b) != 8)
        return false;
    if (BinaryenStoreGetValueType(store_a) != BinaryenTypeFloat64() ||
        BinaryenStoreGetValueType(store_b) != BinaryenTypeFloat64())
        return false;
    if (BinaryenStoreGetOffset(store_a) != 0 ||
        BinaryenStoreGetOffset(store_b) != 0)
        return false;

    int32_t offset_a = 0;
    int32_t offset_b = 0;
    auto *ptr_a = as_offset_ptr(BinaryenStoreGetPtr(store_a), offset_a);
    auto *ptr_b = as_offset_ptr(BinaryenStoreGetPtr(store_b), offset_b);
    if (ptr_a == nullptr || ptr_b == nullptr) return false;
    if (offset_b != offset_a + 8) return false;

    auto *base_a = BinaryenBinaryGetLeft(ptr_a);
    auto *base_b = BinaryenBinaryGetLeft(ptr_b);
    if (!expressions_structurally_equal(base_a, base_b)) return false;

    base_out = ptr_a;
    offset_out = offset_a;
    return true;
}

auto make_replace_lane_pack(BinaryenModuleRef mod,
                            BinaryenExpressionRef value_a,
                            BinaryenExpressionRef value_b)
    -> BinaryenExpressionRef {
    std::array<uint8_t, 16> zero_bytes{};
    auto *zero_vec =
        BinaryenConst(mod, BinaryenLiteralVec128(zero_bytes.data()));
    auto *packed = BinaryenSIMDReplace(mod, BinaryenReplaceLaneVecF64x2(),
                                       zero_vec, 0, value_a);
    return BinaryenSIMDReplace(mod, BinaryenReplaceLaneVecF64x2(), packed, 1,
                               value_b);
}

auto try_vectorize_sincos_pair(const VectorizeContext &ctx,
                               BinaryenExpressionRef expr_a,
                               BinaryenExpressionRef expr_b,
                               std::vector<BinaryenExpressionRef> &side_effects,
                               uint8_t &lane_a, uint8_t &lane_b)
    -> std::optional<BinaryenIndex> {
    if (BinaryenExpressionGetId(expr_a) != BinaryenCallId() ||
        BinaryenExpressionGetId(expr_b) != BinaryenCallId())
        return std::nullopt;

    const std::string_view target_a{BinaryenCallGetTarget(expr_a)};
    const std::string_view target_b{BinaryenCallGetTarget(expr_b)};

    const auto a_is_sin = target_a == "wasmwasm_sin";
    const auto a_is_cos = target_a == "wasmwasm_cos";
    const auto b_is_sin = target_b == "wasmwasm_sin";
    const auto b_is_cos = target_b == "wasmwasm_cos";
    if (!((a_is_sin && b_is_cos) || (a_is_cos && b_is_sin)))
        return std::nullopt;

    if (BinaryenCallGetNumOperands(expr_a) != 1 ||
        BinaryenCallGetNumOperands(expr_b) != 1)
        return std::nullopt;
    if (BinaryenCallIsReturn(expr_a) || BinaryenCallIsReturn(expr_b))
        return std::nullopt;

    auto *arg_a = BinaryenCallGetOperandAt(expr_a, 0);
    auto *arg_b = BinaryenCallGetOperandAt(expr_b, 0);
    if (!expressions_structurally_equal(arg_a, arg_b)) return std::nullopt;

    lane_a = a_is_sin ? 0 : 1;
    lane_b = b_is_sin ? 0 : 1;

    auto *splat = BinaryenUnary(ctx.mod, BinaryenSplatVecF64x2(), arg_a);
    auto splat_index = make_temp_v128(ctx, splat, side_effects);

    std::array<BinaryenExpressionRef, 1> call_args{
        get_temp_v128(ctx, splat_index)};
    auto *vec_call = BinaryenCall(ctx.mod, "wasmwasm_sincos_x2",
                                  call_args.data(), 1, BinaryenTypeVec128());
    return make_temp_v128(ctx, vec_call, side_effects);
}

void try_vectorize_block(BinaryenModuleRef mod, BinaryenFunctionRef fn,
                         BinaryenExpressionRef block) {
    const VectorizeContext ctx{.mod = mod, .fn = fn};
    auto count = BinaryenBlockGetNumChildren(block);
    if (count < 2) return;

    for (BinaryenIndex i = 0; i + 1 < count; ++i) {
        auto *stmt_a = BinaryenBlockGetChildAt(block, i);
        auto *stmt_b = BinaryenBlockGetChildAt(block, i + 1);

        const auto cand_a = match_candidate_expr(stmt_a);
        const auto cand_b = match_candidate_expr(stmt_b);
        if (cand_a.value != nullptr && cand_b.value != nullptr &&
            BinaryenExpressionGetType(cand_a.value) == BinaryenTypeFloat64() &&
            BinaryenExpressionGetType(cand_b.value) == BinaryenTypeFloat64()) {
            std::vector<BinaryenExpressionRef> side_effects;
            auto vec_index = try_vectorize_expr_pair(
                ctx, cand_a.value, cand_b.value, side_effects);
            if (vec_index.has_value()) {
                auto *lane0 =
                    BinaryenSIMDExtract(mod, BinaryenExtractLaneVecF64x2(),
                                        get_temp_v128(ctx, *vec_index), 0);
                auto *lane1 =
                    BinaryenSIMDExtract(mod, BinaryenExtractLaneVecF64x2(),
                                        get_temp_v128(ctx, *vec_index), 1);

                auto *new_a = lane0;
                auto *new_b = lane1;
                if (cand_a.is_local_set)
                    new_a = BinaryenLocalSet(mod, cand_a.local_index, lane0);
                if (cand_b.is_local_set)
                    new_b = BinaryenLocalSet(mod, cand_b.local_index, lane1);

                auto insert_at = i;
                for (auto *effect : side_effects) {
                    BinaryenBlockInsertChildAt(block, insert_at, effect);
                    ++insert_at;
                    ++count;
                }
                BinaryenBlockSetChildAt(block, insert_at, new_a);
                BinaryenBlockInsertChildAt(block, insert_at + 1, new_b);
                ++count;

                i = insert_at + 1;
                continue;
            }

            std::vector<BinaryenExpressionRef> sincos_side_effects;
            uint8_t lane_a = 0;
            uint8_t lane_b = 0;
            auto sincos_index =
                try_vectorize_sincos_pair(ctx, cand_a.value, cand_b.value,
                                          sincos_side_effects, lane_a, lane_b);
            if (sincos_index.has_value()) {
                auto *extract_a = BinaryenSIMDExtract(
                    mod, BinaryenExtractLaneVecF64x2(),
                    get_temp_v128(ctx, *sincos_index), lane_a);
                auto *extract_b = BinaryenSIMDExtract(
                    mod, BinaryenExtractLaneVecF64x2(),
                    get_temp_v128(ctx, *sincos_index), lane_b);

                auto *new_a = extract_a;
                auto *new_b = extract_b;
                if (cand_a.is_local_set)
                    new_a =
                        BinaryenLocalSet(mod, cand_a.local_index, extract_a);
                if (cand_b.is_local_set)
                    new_b =
                        BinaryenLocalSet(mod, cand_b.local_index, extract_b);

                auto insert_at = i;
                for (auto *effect : sincos_side_effects) {
                    BinaryenBlockInsertChildAt(block, insert_at, effect);
                    ++insert_at;
                    ++count;
                }
                BinaryenBlockSetChildAt(block, insert_at, new_a);
                BinaryenBlockInsertChildAt(block, insert_at + 1, new_b);
                ++count;

                i = insert_at + 1;
                continue;
            }
        }

        if (BinaryenExpressionGetId(stmt_a) == BinaryenStoreId() &&
            BinaryenExpressionGetId(stmt_b) == BinaryenStoreId()) {
            BinaryenExpressionRef base_ptr = nullptr;
            int32_t base_offset = 0;
            if (stores_are_vectorizable_pair(stmt_a, stmt_b, base_ptr,
                                             base_offset)) {
                auto *value_a = BinaryenStoreGetValue(stmt_a);
                auto *value_b = BinaryenStoreGetValue(stmt_b);

                std::vector<BinaryenExpressionRef> side_effects;
                const auto vec_index = try_vectorize_expr_pair(
                    ctx, value_a, value_b, side_effects);

                BinaryenExpressionRef packed = nullptr;
                if (vec_index.has_value()) {
                    packed = get_temp_v128(ctx, *vec_index);
                } else {
                    side_effects.clear();
                    packed = make_replace_lane_pack(mod, value_a, value_b);
                }

                auto *vec_store = BinaryenStore(mod, 16, 0, 8, base_ptr, packed,
                                                BinaryenTypeVec128(), "0");

                auto insert_at = i;
                for (auto *effect : side_effects) {
                    BinaryenBlockInsertChildAt(block, insert_at, effect);
                    ++insert_at;
                    ++count;
                }
                BinaryenBlockSetChildAt(block, insert_at, vec_store);
                BinaryenBlockSetChildAt(block, insert_at + 1, BinaryenNop(mod));

                i = insert_at + 1;
                continue;
            }
        }
    }
}

void visit(BinaryenModuleRef mod, BinaryenFunctionRef fn,
           BinaryenExpressionRef expr) {
    if (expr == nullptr) return;
    const auto id = BinaryenExpressionGetId(expr);
    if (id == BinaryenBlockId()) {
        auto count = BinaryenBlockGetNumChildren(expr);
        for (BinaryenIndex i = 0; i < count; ++i)
            visit(mod, fn, BinaryenBlockGetChildAt(expr, i));
        try_vectorize_block(mod, fn, expr);
    } else if (id == BinaryenIfId()) {
        visit(mod, fn, BinaryenIfGetIfTrue(expr));
        visit(mod, fn, BinaryenIfGetIfFalse(expr));
    } else if (id == BinaryenLoopId()) {
        visit(mod, fn, BinaryenLoopGetBody(expr));
    }
}

} // namespace

void run_simd_vectorization_pass(BinaryenModuleRef mod) {
    const auto num_functions = BinaryenGetNumFunctions(mod);
    for (BinaryenIndex i = 0; i < num_functions; ++i) {
        auto *fn = BinaryenGetFunctionByIndex(mod, i);
        visit(mod, fn, BinaryenFunctionGetBody(fn));
    }
}
