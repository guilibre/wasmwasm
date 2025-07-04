cmake_minimum_required(VERSION 3.14)
project(wasmwasm)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(FetchContent)

set(BUILD_STATIC_LIB ON)

FetchContent_Declare(
    binaryen
    GIT_REPOSITORY https://github.com/WebAssembly/binaryen.git
    GIT_TAG main
)
option(BUILD_TESTS "Build GTest-based tests" OFF)
option(BUILD_LLVM_DWARF "Enable full DWARF support" ON)
FetchContent_MakeAvailable(binaryen)

add_compile_options(
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
)

include_directories(includes)

file(GLOB_RECURSE SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

add_library(${PROJECT_NAME}_lib STATIC ${SRC_FILES})
target_link_libraries(${PROJECT_NAME}_lib binaryen)
target_include_directories(${PROJECT_NAME}_lib PRIVATE
    ${binaryen_SOURCE_DIR}/src
    ./include
)
set_target_properties(${PROJECT_NAME}_lib PROPERTIES
    CMAKE_CXX_STANDARD 23
    CMAKE_CXX_STANDARD_REQUIRED ON
)
set_property(TARGET ${PROJECT_NAME}_lib PROPERTY INTERPROCEDURAL_OPTIMIZATION $<$<CONFIG:Release>:ON>)
target_compile_options(${PROJECT_NAME}_lib PRIVATE
    $<$<CONFIG:Debug>:-O0 -g>
    $<$<CONFIG:Release>:-O2 -DNDEBUG>
)

add_executable(${PROJECT_NAME} app/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib)
target_include_directories(${PROJECT_NAME} PRIVATE src)
set_target_properties(${PROJECT_NAME} PROPERTIES
    CMAKE_CXX_STANDARD 23
    CMAKE_CXX_STANDARD_REQUIRED ON
)
target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CONFIG:Debug>:-O0 -g>
    $<$<CONFIG:Release>:-O2 -DNDEBUG>
)

if(CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
    target_compile_options(${PROJECT_NAME}_lib PRIVATE
        $<$<CONFIG:Debug>:-O0 -g -gsource-map>
        $<$<CONFIG:Release>:-O2 -DNDEBUG>
    )
    target_compile_options(${PROJECT_NAME} PRIVATE
        $<$<CONFIG:Debug>:-O0 -g -gsource-map>
        $<$<CONFIG:Release>:-O2 -DNDEBUG>
    )
    target_link_options(${PROJECT_NAME} PRIVATE
        -sENVIRONMENT=web
        -sNO_EXIT_RUNTIME=1
        -sEXPORT_ES6=1
        -sEXPORTED_FUNCTIONS=["_main"]
        -sEXPORTED_RUNTIME_METHODS=["callMain","FS","FS_readFile"]
        -sFORCE_FILESYSTEM
        -sINVOKE_RUN=0
    )
    set_property(TARGET ${PROJECT_NAME} PROPERTY INTERPROCEDURAL_OPTIMIZATION $<$<CONFIG:Release>:ON>)
endif()