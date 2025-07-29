#!/bin/bash

set -e

cd compiler

emcmake cmake -S . -B build_wasm -DCMAKE_BUILD_TYPE=Release

cmake --build build_wasm

cd ../website
mkdir -p public
cp ../compiler/build_wasm/math.wasm public/
cp ../compiler/build_wasm/wasmwasm.wasm src/wasmwasm
cp ../compiler/build_wasm/wasmwasm.js src/wasmwasm

echo "Build complete and files copied."
