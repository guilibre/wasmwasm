param(
    [Parameter(Position=0)]
    [int]$Jobs = 0
)

$ErrorActionPreference = "Stop"

try {
    Set-Location -Path "compiler"
    emcmake cmake -S . -B build -DBUILD_DEBUGGER=OFF
    if ($Jobs -gt 0) {
        cmake --build build -- "-j$Jobs"
    } else {
        cmake --build build
    }
    Set-Location -Path "..\frontend"
    if (!(Test-Path "public")) { New-Item -ItemType Directory -Path "public" | Out-Null }
    Copy-Item "..\compiler\build\math\math.wasm" "public\"
    Copy-Item "..\compiler\build\app\wasmwasm.wasm" "src\wasmwasm\"
    Copy-Item "..\compiler\build\app\wasmwasm.js" "src\wasmwasm\"
    Write-Host "Build complete and files copied."
    Set-Location -Path "..\"
}
catch {
    Write-Host "Error: $_"
    exit 1
}