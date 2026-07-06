#!/bin/bash

set -e

JOBS=${1:-0}

cd patch_compiler
emcmake cmake -S . -B build -DBUILD_TESTS_WW=OFF
if [ "$JOBS" -gt 0 ]; then
    cmake --build build -- -j$JOBS
else
    cmake --build build
fi

# cd ../conductor_compiler
# emcmake cmake -S . -B build
# if [ "$JOBS" -gt 0 ]; then
#     cmake --build build -- -j$JOBS
# else
#     cmake --build build
# fi

# cd ../score_compiler
# emcmake cmake -S . -B build
# if [ "$JOBS" -gt 0 ]; then
#     cmake --build build -- -j$JOBS
# else
#     cmake --build build
# fi

cd ../frontend

cp ../patch_compiler/build/math/math.wasm public/

cp ../patch_compiler/build/app/wasmwasm.wasm src/wasmwasm
cp ../patch_compiler/build/app/wasmwasm.js src/wasmwasm

# cp ../conductor_compiler/build/app/conductorwasm.wasm src/conductorwasm
# cp ../conductor_compiler/build/app/conductorwasm.js src/conductorwasm

# cp ../score_compiler/build/app/scorewasm.wasm src/scorewasm
# cp ../score_compiler/build/app/scorewasm.js src/scorewasm

echo "Build complete and files copied."
