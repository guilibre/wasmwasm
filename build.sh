#!/bin/bash

set -e

JOBS=${1:-0}

cd patch_compiler

emcmake cmake -S . -B build -DBUILD_DEBUGGER=OFF -DBUILD_TESTS_WW=OFF -DCMAKE_BUILD_TYPE=Release
if [ "$JOBS" -gt 0 ]; then
    cmake --build build -- -j$JOBS
else
    cmake --build build
fi

cd ../frontend
mkdir -p public
cp ../patch_compiler/build/math/math.wasm public/
cp ../patch_compiler/build/app/wasmwasm.wasm src/wasmwasm
cp ../patch_compiler/build/app/wasmwasm.js src/wasmwasm

echo "Build complete and files copied."
