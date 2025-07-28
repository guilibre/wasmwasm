#include "type.hpp"
#include <variant>

auto Type::to_binaryen_type() -> BinaryenType {
    if (auto *base = std::get_if<TypeBase>(&node)) {
        switch (base->kind) {
        case BaseTypeKind::Bool:
            return BinaryenTypeInt32();
        case BaseTypeKind::Float:
            return BinaryenTypeFloat64();
        case BaseTypeKind::Int:
            return BinaryenTypeInt32();
        case BaseTypeKind::Void:
            return BinaryenTypeNone();
        }
    }
    if (std::holds_alternative<TypeVar>(node)) return BinaryenTypeFloat64();

    return BinaryenTypeInt32();
}