cmake_minimum_required(VERSION 3.14)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif()

project(wasmwasm VERSION 0.1 LANGUAGES CXX)

set(CMAKE_BUILD_TYPE Release CACHE STRING "")
set(BUILD_STATIC_LIB ON CACHE BOOL "" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(BUILD_OBJECT_LIBS OFF CACHE BOOL "" FORCE)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE BOOL "" FORCE)

include(cmake/compiler-flags.cmake)

include(FetchContent)

fetchcontent_declare(
    binaryen
    GIT_REPOSITORY https://github.com/WebAssembly/binaryen.git
    GIT_TAG version_130
)
set(BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(BUILD_TOOLS OFF CACHE BOOL "" FORCE)
set(EMSCRIPTEN_ENABLE_WASM_EH ON CACHE BOOL "" FORCE)
fetchcontent_makeavailable(binaryen)

fetchcontent_declare(
    jsoncpp
    GIT_REPOSITORY https://github.com/open-source-parsers/jsoncpp.git
    GIT_TAG 1.9.8
)
set(JSONCPP_WITH_CMAKE_PACKAGE OFF CACHE BOOL "" FORCE)
set(JSONCPP_WITH_INSTALL OFF CACHE BOOL "" FORCE)
set(JSONCPP_WITH_PKGCONFIG_SUPPORT OFF CACHE BOOL "" FORCE)
set(JSONCPP_WITH_POST_BUILD_UNITTEST OFF CACHE BOOL "" FORCE)
set(JSONCPP_WITH_TESTS OFF CACHE BOOL "" FORCE)
fetchcontent_makeavailable(jsoncpp)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(includes)

add_subdirectory(math)
add_subdirectory(src)
add_subdirectory(app)

option(BUILD_TESTS_WW "Build the ww compiler test suite" OFF)
if(BUILD_TESTS_WW)
    enable_testing()
    add_subdirectory(tests)
endif()
