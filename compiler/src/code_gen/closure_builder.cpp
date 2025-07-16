#include "closure_builder.hpp"

ClosureBuilder::ClosureBuilder(BinaryenModuleRef module, size_t heap_top)
    : module(module), heap_top(heap_top) {}

auto ClosureBuilder::build(BinaryenExpressionRef func_index,
                           const std::vector<BinaryenExpressionRef> &captures)
    -> BinaryenExpressionRef {
    int32_t env_size = captures.size() * 8;
    int32_t env_ptr_val = heap_top;
    heap_top += env_size;

    auto *env_ptr = BinaryenConst(module, BinaryenLiteralInt32(env_ptr_val));

    std::vector<BinaryenExpressionRef> stores;
    stores.reserve(captures.size() + 3);

    for (size_t i = 0; i < captures.size(); ++i) {
        auto *offset = BinaryenConst(module, BinaryenLiteralInt32(i * 8));
        auto *addr =
            BinaryenBinary(module, BinaryenAddInt32(), env_ptr, offset);

        stores.push_back(BinaryenStore(module, 8, 0, 8, addr, captures[i],
                                       BinaryenTypeFloat64(), "memory"));
    }

    int32_t closure_ptr_val = heap_top;
    heap_top += 8;
    auto *closure_ptr =
        BinaryenConst(module, BinaryenLiteralInt32(closure_ptr_val));

    stores.push_back(BinaryenStore(module, 4, 0, 4, closure_ptr, func_index,
                                   BinaryenTypeInt32(), "memory"));

    stores.push_back(BinaryenStore(module, 4, 4, 4, closure_ptr, env_ptr,
                                   BinaryenTypeInt32(), "memory"));

    stores.push_back(closure_ptr);

    return BinaryenBlock(module, nullptr, stores.data(), stores.size(),
                         BinaryenTypeInt32());
}