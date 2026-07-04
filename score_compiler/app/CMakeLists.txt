add_executable(${PROJECT_NAME} bindings.cpp)

target_link_options(${PROJECT_NAME} PRIVATE
    --bind
    -sENVIRONMENT=web
    -sEXPORT_ES6=1
    -sINITIAL_MEMORY=33554432
    -sINVOKE_RUN=0
    -sSTACK_SIZE=5242880
    -sWASM=1
)

target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib compiler_flags)
target_include_directories(${PROJECT_NAME} PRIVATE ../src)
