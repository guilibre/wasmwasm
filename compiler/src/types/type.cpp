#include "type.hpp"

auto Type::to_binaryen_type() -> BinaryenType {
    if (auto *base = std::get_if<TypeBase>(&node)) {
        switch (base->kind) {
        case BaseTypeKind::Float:
            return BinaryenTypeFloat64();
        case BaseTypeKind::Int:
        case BaseTypeKind::Bool:
            return BinaryenTypeInt32();
        case BaseTypeKind::Void:
            return BinaryenTypeNone();
        }
    }

    return BinaryenTypeInt32();
}