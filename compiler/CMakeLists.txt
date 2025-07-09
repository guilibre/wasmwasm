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
    GIT_TAG version_123
)
FetchContent_MakeAvailable(binaryen)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set( COMPILE_OPTIONS
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
    $<$<CONFIG:Release>:-O2 -DNDEBUG>
)

add_executable(${PROJECT_NAME} app/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib binaryen)
target_include_directories(${PROJECT_NAME} PRIVATE
    ${binaryen_SOURCE_DIR}
    src
)
target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CONFIG:Debug>:-O0 -g>
    $<$<CONFIG:Release>:-O2 -DNDEBUG>
)

if(CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
    target_include_directories(${PROJECT_NAME}_lib PRIVATE
        /home/gui/src/emsdk/upstream/emscripten/cache/sysroot/include/
    )
    target_compile_options(${PROJECT_NAME}_lib PRIVATE
        -O2 -DNDEBUG
        ${COMPILE_OPTIONS}
    )
    target_compile_options(${PROJECT_NAME} PRIVATE
        -O2 -DNDEBUG
        ${COMPILE_OPTIONS}
    )
    target_link_options(${PROJECT_NAME} PRIVATE
        -sENVIRONMENT=web
        -sEXPORT_ES6=1
        -sEXPORTED_FUNCTIONS=["_free","_run_compiler","_malloc"]
        -sEXPORTED_RUNTIME_METHODS=["FS_readFile","FS","HEAPF32","HEAPU8","lengthBytesUTF8","stringToUTF8"]
        -sFORCE_FILESYSTEM
        -sINVOKE_RUN=0
        -sWASM=1
        -sMAIN_MODULE=2
        -g1 -O2
    )

    add_executable(functions functions/sin.cpp)
    target_compile_options(functions PRIVATE
        -O2 -ffast-math -msimd128 -g1
        ${COMPILE_OPTIONS}
    )
    target_link_options(functions PRIVATE
        -sEXPORTED_FUNCTIONS=["_wasmwasm_sin"]
        -sWASM=1
        -O2 -ffast-math -msimd128 -g1
    )
endif()