add_executable(math basic.cpp)
target_link_options(math PRIVATE
    --no-entry
    -sEXPORTED_FUNCTIONS=["_wasmwasm_sin","_wasmwasm_cos","_wasmwasm_clip","_wasmwasm_fract","_wasmwasm_sign","_wasmwasm_exp"]
    -sSTANDALONE_WASM=1
)
