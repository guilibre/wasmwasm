cmake_minimum_required(VERSION 3.14)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif()

project(wasmwasm VERSION 0.1 LANGUAGES CXX)

set(BUILD_STATIC_LIB ON)

include(cmake/compiler-flags.cmake)

include(FetchContent)

fetchcontent_declare(
    binaryen
    GIT_REPOSITORY https://github.com/WebAssembly/binaryen.git
    GIT_TAG version_130
)
set(BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(BUILD_TOOLS OFF CACHE BOOL "" FORCE)
fetchcontent_makeavailable(binaryen)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(includes)

add_subdirectory(math)
add_subdirectory(src)
add_subdirectory(app)
if(BUILD_DEBUGGER)
    add_subdirectory(debugger)
endif()
