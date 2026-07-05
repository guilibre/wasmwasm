add_library(${PROJECT_NAME}_lib STATIC
    builtins.cpp
    compile.cpp
    ast/ast.cpp
    backend/binaryen/binaryen_backend.cpp
    backend/binaryen/binaryen_codegen.cpp
    backend/binaryen/binaryen_emit.cpp
    backend/binaryen/binaryen_emit_instances.cpp
    backend/binaryen/binaryen_emit_main_loop.cpp
    backend/binaryen/binaryen_math_module.cpp
    backend/binaryen/instance_layout.cpp
    ir/lower.cpp
    ir/lower_arrays.cpp
    ir/lower_bind.cpp
    ir/lower_call.cpp
    ir/lower_closures.cpp
    ir/lower_expr.cpp
    ir/lower_unroll.cpp
    lsp/lsp.cpp
    parser/parser.cpp
    parser/tokenizer.cpp
    routing/routing.cpp
    types/type_inference.cpp
    types/type.cpp
)

target_link_libraries(${PROJECT_NAME}_lib PRIVATE binaryen jsoncpp_static compiler_flags)
target_include_directories(${PROJECT_NAME}_lib PRIVATE ${binaryen_SOURCE_DIR}/src ./)
