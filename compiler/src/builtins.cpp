#include "builtins.hpp"

#include "types/type.hpp"

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>> {
    const auto int_type = Type::make<TypeBase>(BaseTypeKind::Int);
    const auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
    const auto void_type = Type::make<TypeBase>(BaseTypeKind::Void);
    const auto int_to_float = Type::make<TypeFun>(int_type, float_type);
    const auto float_to_float = Type::make<TypeFun>(float_type, float_type);
    const auto float_to_float_to_float =
        Type::make<TypeFun>(float_type, float_to_float);
    return {{
        {"PI", float_type},
        {"SAMPLE_RATE", float_type},
        {"cos", float_to_float},
        {"sin", float_to_float},
        {"sign", float_to_float},
        {"fract", float_to_float},
        {"clip", float_to_float},
        {"exp", float_to_float},
        {"uniform", float_to_float_to_float},
        {"gaussian", float_to_float_to_float},
        {"floor", float_to_float},
        {"ceil", float_to_float},
        {"sqrt", float_to_float},
        {"round", float_to_float},
        {"log", float_to_float},
        {"buffer",
         Type::make<TypeFun>(
             float_type,
             Type::make<TypeFun>(
                 int_type, Type::make<TypeFun>(int_to_float, void_type)))},
    }};
}
