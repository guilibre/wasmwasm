add_library(${PROJECT_NAME}_lib STATIC
    compile.cpp
    ast/ast.cpp
    backend/codegen.cpp
    lsp/lsp.cpp
    parser/parser.cpp
    parser/tokenizer.cpp
    resolve/resolver.cpp
    resolve/optimize.cpp
)

target_link_libraries(${PROJECT_NAME}_lib PRIVATE compiler_flags)
target_include_directories(${PROJECT_NAME}_lib PRIVATE ${binaryen_SOURCE_DIR}/src ./)
