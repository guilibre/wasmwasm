#include "code_gen_context.hpp"

#include "binaryen-c.h"

#include <cassert>

auto BinaryenVariable::get_local(BinaryenModuleRef module) const
    -> BinaryenExpressionRef {
    return BinaryenLocalGet(module, local, type);
}

auto BinaryenVariable::set_local(BinaryenModuleRef module,
                                 BinaryenExpressionRef value) const
    -> BinaryenExpressionRef {
    return BinaryenLocalSet(module, local, value);
}

CodeGenContext::CodeGenContext(BinaryenModuleRef module) : module(module) {}