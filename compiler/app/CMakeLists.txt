add_executable(${PROJECT_NAME} bindings.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib binaryen)
target_link_options(${PROJECT_NAME} PRIVATE
    --bind
    -sALLOW_MEMORY_GROWTH=1
    -sENVIRONMENT=web
    -sEXPORT_ES6=1
    -sINVOKE_RUN=0
    -sNO_DISABLE_EXCEPTION_CATCHING
    -sWASM=1
)
target_include_directories(${PROJECT_NAME} PUBLIC
    ${binaryen_SOURCE_DIR}/src
    ../src
)
