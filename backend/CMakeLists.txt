cmake_minimum_required(VERSION 3.20)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif()

project(
    "backwasm"
    VERSION 0.1
    LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
endif()

set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(BUILD_STATIC_LIBS OFF CACHE BOOL "" FORCE)

option(SODIUM_FORCE_FETCHCONTENT "Build libsodium from source via FetchContent, even if present on the system" OFF)
option(JWT_CPP_FORCE_FETCHCONTENT "Build jwt-cpp from source via FetchContent, even if present on the system" OFF)

include(cmake/CompilerFlags.cmake)

include(FetchContent)

find_package(PkgConfig REQUIRED)
pkg_check_modules(UUID REQUIRED)
pkg_check_modules(zlib REQUIRED)
find_package(OpenSSL REQUIRED)

FetchContent_Declare(
    crow
    GIT_REPOSITORY https://github.com/CrowCpp/Crow
    GIT_TAG v1.3.3
    GIT_SHALLOW ON
)
set(CROW_INSTALL OFF CACHE BOOL "" FORCE)
set(CROW_ENABLE_SSL ON CACHE BOOL "" FORCE)
set(CROW_ENABLE_COMPRESSION ON CACHE BOOL "" FORCE)
set(CROW_ENABLE_TSAN ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(crow)

if(NOT SODIUM_FORCE_FETCHCONTENT)
    pkg_check_modules(SODIUM libsodium IMPORTED_TARGET)
endif()
if(SODIUM_FOUND AND NOT SODIUM_FORCE_FETCHCONTENT)
    add_library(sodium ALIAS PkgConfig::SODIUM)
else()
    FetchContent_Declare(
        libsodium_cmake
        GIT_REPOSITORY https://github.com/robinlinden/libsodium-cmake
        GIT_TAG 9b2848dfc1b917a9410f0de9d81059b26cbfaa8d
        GIT_SHALLOW ON
    )
    set(SODIUM_DISABLE_TESTS ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(libsodium_cmake)
endif()

if(NOT JWT_CPP_FORCE_FETCHCONTENT)
    find_package(jwt-cpp CONFIG QUIET)
endif()
if(NOT jwt-cpp_FOUND OR JWT_CPP_FORCE_FETCHCONTENT)
    FetchContent_Declare(
        jwt-cpp
        GIT_REPOSITORY https://github.com/Thalhammer/jwt-cpp
        GIT_TAG v0.7.2
        GIT_SHALLOW ON
    )
    set(JWT_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(jwt-cpp)
endif()

find_library(
    OCCI REQUIRED
    NAMES libocci.so
    PATHS ${CMAKE_SOURCE_DIR}/vendor/instantclient
)
find_library(
    CLNTSH REQUIRED
    NAMES libclntsh.so
    PATHS ${CMAKE_SOURCE_DIR}/vendor/instantclient
)

add_subdirectory(src)
