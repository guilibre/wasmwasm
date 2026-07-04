get_filename_component(EM_BIN_DIR "${CMAKE_CXX_COMPILER}" DIRECTORY)
get_filename_component(EMSDK_UPSTREAM "${EM_BIN_DIR}/.." ABSOLUTE)
set(WASM_CLANG "${EMSDK_UPSTREAM}/bin/clang++")
set(WASM_SYSROOT "${EMSDK_UPSTREAM}/emscripten/cache/sysroot")

set(MATH_WASM "${CMAKE_CURRENT_BINARY_DIR}/math.wasm")

add_custom_command(
    OUTPUT ${MATH_WASM}
    COMMAND ${WASM_CLANG}
    --target=wasm32
    --no-standard-libraries
    -Wl,--no-entry
    -Wl,--export=wasmwasm_abs
    -Wl,--export=wasmwasm_ceil
    -Wl,--export=wasmwasm_clip
    -Wl,--export=wasmwasm_cos
    -Wl,--export=wasmwasm_exp
    -Wl,--export=wasmwasm_floor
    -Wl,--export=wasmwasm_fract
    -Wl,--export=wasmwasm_gaussian
    -Wl,--export=wasmwasm_log
    -Wl,--export=wasmwasm_max
    -Wl,--export=wasmwasm_min
    -Wl,--export=wasmwasm_pow
    -Wl,--export=wasmwasm_round
    -Wl,--export=wasmwasm_sign
    -Wl,--export=wasmwasm_sin
    -Wl,--export=wasmwasm_sqrt
    -Wl,--export=wasmwasm_tan
    -Wl,--export=wasmwasm_tanh
    -Wl,--export=wasmwasm_uniform
    -Wl,--lto-O3
    -Wl,--gc-sections
    -isystem ${WASM_SYSROOT}/include/c++/v1
    -isystem ${WASM_SYSROOT}/include/wasm32-emscripten/c++/v1
    -isystem ${WASM_SYSROOT}/include
    -O3 -flto -ffast-math -finline-functions
    -fdata-sections -ffunction-sections
    -msimd128 -mbulk-memory -mmutable-globals
    -mnontrapping-fptoint -msign-ext -mtail-call
    -std=c++26
    -o ${MATH_WASM}
    ${CMAKE_CURRENT_SOURCE_DIR}/basic.cpp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/basic.cpp
    COMMENT "Compilando math.wasm com clang --target=wasm32"
)

add_custom_target(math ALL DEPENDS ${MATH_WASM})
