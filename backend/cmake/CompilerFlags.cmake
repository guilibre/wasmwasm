set(CMAKE_CXX_FLAGS_RELEASE
    "-O3 -DNDEBUG -fno-math-errno -fno-trapping-math -fomit-frame-pointer -funroll-loops -fvisibility=hidden -fvisibility-inlines-hidden -flto"
    CACHE STRING "" FORCE
)
set(CMAKE_EXE_LINKER_FLAGS_RELEASE
    "-flto"
    CACHE STRING "" FORCE
)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON)

set(CMAKE_CXX_FLAGS_DEBUG
    "-O0 -g3 -DDEBUG -fno-omit-frame-pointer -fno-inline -D_GLIBCXX_ASSERTIONS -fsanitize=address,undefined -fsanitize-address-use-after-scope -fno-sanitize-recover=undefined"
    CACHE STRING "" FORCE
)
set(CMAKE_EXE_LINKER_FLAGS_DEBUG
    "-fsanitize=address,undefined"
    CACHE STRING "" FORCE
)

add_library(compiler_flags INTERFACE)

target_compile_options(compiler_flags INTERFACE
    -Wall
    -Wextra
    -Wpedantic
    -Wshadow
    -Wnon-virtual-dtor
    -Wcast-align
    -Wunused
    -Woverloaded-virtual
    -Wconversion
    -Wsign-conversion
    -Wmisleading-indentation
    -Wnull-dereference
    -Wdouble-promotion
    -Wno-missing-field-initializers
    -Wformat=2
    -Werror=return-type
    -Werror=uninitialized
)
