add_executable(ww_debug main.cpp)
target_link_libraries(ww_debug wasmwasm_lib)
target_link_options(ww_debug PRIVATE -sNO_DISABLE_EXCEPTION_CATCHING)
target_include_directories(${PROJECT_NAME} PUBLIC
    ../src
)
