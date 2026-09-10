option(WW_CLANGD_EMSCRIPTEN_INCLUDES "Add emscripten sysroot -isystem flags to compile_commands.json for clangd" ON)
if(WW_CLANGD_EMSCRIPTEN_INCLUDES)
    find_program(WW_EM_CONFIG em-config)
    if(WW_EM_CONFIG)
        execute_process(
            COMMAND ${WW_EM_CONFIG} CACHE
            OUTPUT_VARIABLE WW_EM_CACHE_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        set(WW_EM_SYSROOT "${WW_EM_CACHE_DIR}/sysroot")
    endif()

    if(WW_EM_SYSROOT AND EXISTS "${WW_EM_SYSROOT}/include")
        add_compile_options($<$<COMPILE_LANGUAGE:CXX>:--target=wasm32>)
    endif()

    foreach(WW_EM_INCLUDE_DIR
        "${WW_EM_SYSROOT}/include/c++/v1"
        "${WW_EM_SYSROOT}/include/wasm32-emscripten/c++/v1"
        "${WW_EM_SYSROOT}/include"
    )
        if(EXISTS "${WW_EM_INCLUDE_DIR}")
            add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-isystem${WW_EM_INCLUDE_DIR}>)
        endif()
    endforeach()
endif()

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
