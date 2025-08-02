$ErrorActionPreference = "Stop"

try {
    Set-Location -Path "compiler"
    emcmake cmake -S . -B build_wasm -DCMAKE_BUILD_TYPE=Release
    cmake --build build_wasm
    Set-Location -Path "..\website"
    if (!(Test-Path "public")) { New-Item -ItemType Directory -Path "public" | Out-Null }
    Copy-Item "..\compiler\build_wasm\math.wasm" "public\"
    Copy-Item "..\compiler\build_wasm\wasmwasm.wasm" "src\wasmwasm\"
    Copy-Item "..\compiler\build_wasm\wasmwasm.js" "src\wasmwasm\"
    Write-Host "Build complete and files copied."
    Set-Location -Path "..\"
}
catch {
    Write-Host "Error: $_"
    exit 1
}