add_executable(ww_debug main.cpp)

target_link_options(ww_debug PRIVATE
    --preload-file ${CMAKE_BINARY_DIR}/math/math.wasm@/math.wasm
    -sSTACK_SIZE=5242880
)

target_link_libraries(ww_debug PRIVATE ${PROJECT_NAME}_lib binaryen jsoncpp_static compiler_flags)
target_include_directories(ww_debug PRIVATE ${binaryen_SOURCE_DIR}/src ../src)
