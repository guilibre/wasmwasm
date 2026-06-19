#!/bin/bash

set -e

cd compiler

emcmake cmake -S . -B build -DBUILD_DEBUGGER=OFF
cmake --build build

cd ../frontend
mkdir -p public
cp ../compiler/build/math/math.wasm public/
cp ../compiler/build/app/wasmwasm.wasm src/wasmwasm
cp ../compiler/build/app/wasmwasm.js src/wasmwasm

echo "Build complete and files copied."
