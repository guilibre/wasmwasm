cmake_minimum_required(VERSION 3.14)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
   set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif()

project(wasmwasm)

set(BUILD_STATIC_LIB ON)

include(FetchContent)
set(BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(BUILD_TOOLS OFF CACHE BOOL "" FORCE)
FetchContent_Declare(
    binaryen
    GIT_REPOSITORY https://github.com/WebAssembly/binaryen.git
    GIT_TAG version_127
)
FetchContent_MakeAvailable(binaryen)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
    add_compile_definitions(
        _CRT_SECURE_NO_WARNINGS
        NOMINMAX
    )
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    set(COMPILE_OPTIONS
        /W4
        /WX
        /permissive-
        /Zc:__cplusplus
    )
else()
    set(COMPILE_OPTIONS
        -Wall
        -Wextra
        -Wpedantic
        -Wconversion
        -Wsign-conversion
        -Wshadow
        -Wold-style-cast
        -Wnon-virtual-dtor
        -Woverloaded-virtual
        -Wnull-dereference
        -Wdouble-promotion
        -Wformat=2
        -Wimplicit-fallthrough
        -fstrict-aliasing
        -fno-common
        -fPIC
    )
endif()

include_directories(includes)

file(GLOB_RECURSE SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

add_library(${PROJECT_NAME}_lib STATIC ${SRC_FILES})
target_link_libraries(${PROJECT_NAME}_lib binaryen)
target_include_directories(${PROJECT_NAME}_lib PUBLIC
    ${binaryen_SOURCE_DIR}/src
    ./include
)
target_compile_options(${PROJECT_NAME}_lib PRIVATE
    $<$<CONFIG:Debug>:-O0 -g>
    $<$<CONFIG:Release>:-O0 -DNDEBUG>
)

add_executable(${PROJECT_NAME} app/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib binaryen)
target_include_directories(${PROJECT_NAME} PRIVATE
    ${binaryen_SOURCE_DIR}
    src
)
target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CONFIG:Debug>:-g -Og>
    $<$<CONFIG:Release>:-DNDEBUG -g0 -O3 -ffast-math -msimd128 -msse4.2 -fno-math-errno>
)

if(CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
    target_compile_options(${PROJECT_NAME}_lib PRIVATE
        ${COMPILE_OPTIONS}
    )
    target_compile_options(${PROJECT_NAME} PRIVATE
        ${COMPILE_OPTIONS}
    )
    target_link_options(${PROJECT_NAME} PRIVATE
        -sASSERTIONS
        -sENVIRONMENT=web
        -sEXPORT_ES6=1
        -sEXPORTED_FUNCTIONS=["_free","_run_compiler","_malloc"]
        -sEXPORTED_RUNTIME_METHODS=["FS_readFile","FS","HEAPF32","HEAPU8","lengthBytesUTF8","stringToUTF8"]
        -sFORCE_FILESYSTEM
        -sINVOKE_RUN=0
        -sWASM=1
        -sMAIN_MODULE=2
        -sALLOW_MEMORY_GROWTH=1
        ${COMPILE_OPTIONS}
    )

    add_executable(math math/basic.cpp)
    target_compile_options(math PRIVATE
        ${COMPILE_OPTIONS}
    )
    target_link_options(math PRIVATE
        -sASSERTIONS
        -sEXPORTED_FUNCTIONS=["_wasmwasm_sin","_wasmwasm_cos","_wasmwasm_clip","_wasmwasm_fract","_wasmwasm_sign","_wasmwasm_exp"]
        -sSTANDALONE_WASM=1
        --no-entry
        ${COMPILE_OPTIONS}
    )
endif()