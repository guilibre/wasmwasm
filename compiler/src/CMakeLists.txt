add_library(${PROJECT_NAME}_lib STATIC
    builtins.cpp
    compile.cpp
    ast/ast.cpp
    ir/emit.cpp
    ir/emit_main_loop.cpp
    ir/lower.cpp
    ir/lower_arrays.cpp
    ir/lower_bind.cpp
    ir/lower_call.cpp
    ir/lower_closures.cpp
    ir/lower_expr.cpp
    lsp/lsp.cpp
    parser/parser.cpp
    parser/tokenizer.cpp
    routing/routing.cpp
    types/type_inference.cpp
    types/type.cpp
)

target_link_libraries(${PROJECT_NAME}_lib PRIVATE binaryen jsoncpp_static compiler_flags)
target_include_directories(${PROJECT_NAME}_lib PRIVATE ${binaryen_SOURCE_DIR}/src ./)
