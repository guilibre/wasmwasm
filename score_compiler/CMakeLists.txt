cmake_minimum_required(VERSION 3.14)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif()

project(scorewasm VERSION 0.1 LANGUAGES CXX)

set(BUILD_STATIC_LIB ON CACHE BOOL "" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(BUILD_OBJECT_LIBS OFF CACHE BOOL "" FORCE)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE BOOL "" FORCE)

include(cmake/compiler-flags.cmake)

include(FetchContent)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(includes)

option(BUILD_TESTS_SCORE "Build score_compiler native tests" OFF)

add_subdirectory(src)
add_subdirectory(app)

if(BUILD_TESTS_SCORE)
    enable_testing()
    add_subdirectory(tests)
endif()
