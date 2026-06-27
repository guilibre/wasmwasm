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
    const auto array_type = Type::make<TypeArray>();
    // foldr : Float -> (Float -> Float -> Float) -> Array -> Float
    const auto foldr_type = Type::make<TypeFun>(
        float_type,
        Type::make<TypeFun>(float_to_float_to_float,
                            Type::make<TypeFun>(array_type, float_type)));
    // map : (Float -> Float) -> Array -> Array
    const auto map_type = Type::make<TypeFun>(
        float_to_float, Type::make<TypeFun>(array_type, array_type));
    // zip : (Float -> Float -> Float) -> Array -> Array -> Array
    const auto zip_type = Type::make<TypeFun>(
        float_to_float_to_float,
        Type::make<TypeFun>(array_type,
                            Type::make<TypeFun>(array_type, array_type)));
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
        {"abs", float_to_float},
        {"tanh", float_to_float},
        {"min", float_to_float_to_float},
        {"max", float_to_float_to_float},
        {"foldr", foldr_type},
        {"map", map_type},
        {"zip", zip_type},
        {"delay", Type::make<TypeFun>(
                      float_type, Type::make<TypeFun>(
                                      int_type, Type::make<TypeFun>(
                                                    int_to_float, void_type)))},
    }};
}
