#!/usr/bin/env bash
# Usage: ./run.sh [source.ww | patch.json]
#        echo "OUT[0] <- sin(TIME)" | ./run.sh
#        cat patch.json | ./run.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/../build/debugger"

INPUT="$([ -n "$1" ] && cat "$1" || cat)"
cd "$BUILD_DIR" && printf '%s' "$INPUT" | node --experimental-wasm-exnref ww_debug.js
