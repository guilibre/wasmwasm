add_compile_options(
    -DNDEBUG
    -fdata-sections
    -ffunction-sections
    -fwasm-exceptions
    -sWASM_LEGACY_EXCEPTIONS=0
    -mbulk-memory
    -mmutable-globals
    -mnontrapping-fptoint
    -msign-ext
    -msimd128
    -mtail-call
    -O3
)
add_link_options(
    -flto
    -fwasm-exceptions
    -sWASM_LEGACY_EXCEPTIONS=0
    -msimd128
    -Wl,--gc-sections
)

add_library(compiler_flags INTERFACE)

target_compile_options(compiler_flags INTERFACE
    -Wall
    -Wcast-align
    -Wconversion
    -Wdouble-promotion
    -Werror=return-type
    -Werror=uninitialized
    -Wextra
    -Wformat=2
    -Wmisleading-indentation
    -Wnon-virtual-dtor
    -Wnull-dereference
    -Woverloaded-virtual
    -Wpedantic
    -Wshadow
    -Wsign-conversion
    -Wunused
)
