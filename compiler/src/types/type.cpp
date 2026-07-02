#include "type.hpp"
#include <stdexcept>
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

    if (std::holds_alternative<TypeArray>(node))
        throw std::runtime_error(
            "TypeArray has no direct Binaryen local/param type; arrays are "
            "destructured into scalar temporaries before touching a local");

    return BinaryenTypeFloat64();
}
