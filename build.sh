#!/bin/bash

set -e

JOBS=${1:-0}

cd compiler

emcmake cmake -S . -B build -DBUILD_DEBUGGER=OFF
if [ "$JOBS" -gt 0 ]; then
    cmake --build build -- -j$JOBS
else
    cmake --build build
fi

cd ../frontend
mkdir -p public
cp ../compiler/build/math/math.wasm public/
cp ../compiler/build/app/wasmwasm.wasm src/wasmwasm
cp ../compiler/build/app/wasmwasm.js src/wasmwasm

echo "Build complete and files copied."
