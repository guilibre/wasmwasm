#!/bin/sh

# Exit on any error
set -e

# Check if ARG1 is provided
if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <BuildType>"
  echo "Example: $0 Release"
  exit 1
fi

BUILD_TYPE="$1"

echo "Building wasmwasm with build type: $BUILD_TYPE"

# Enter wasmwasm directory
cd wasmwasm

# Configure with emcmake
emcmake cmake -S . -B build_wasm -DCMAKE_BUILD_TYPE="$BUILD_TYPE"

# Build with 2 threads
cmake --build build_wasm -- -j2

# Go to frontend and copy files
cd ../frontend
mkdir -p public
cp ../wasmwasm/build_wasm/wasmwasm.wasm src/
cp ../wasmwasm/build_wasm/wasmwasm.js src/

echo "Build complete and files copied."
