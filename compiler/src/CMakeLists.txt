add_library(${PROJECT_NAME}_lib STATIC
    builtins.cpp
    ast/ast.cpp
    ir/emit.cpp
    ir/lower.cpp
    lsp/lsp.cpp
    parser/parser.cpp
    parser/tokenizer.cpp
    types/type_inference.cpp
    types/type.cpp
)
target_link_libraries(${PROJECT_NAME}_lib binaryen)
target_include_directories(${PROJECT_NAME}_lib PUBLIC
    ${binaryen_SOURCE_DIR}/src
    ./include
)
