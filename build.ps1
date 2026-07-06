param(
    [Parameter(Position = 0)]
    [int]$Jobs = 0
)

$ErrorActionPreference = "Stop"

try {
    Set-Location -Path "patch_compiler"
    emcmake cmake -S . -B build -DBUILD_TESTS_WW=OFF
    if ($Jobs -gt 0) {
        cmake --build build -- "-j$Jobs"
    }
    else {
        cmake --build build
    }

    # Set-Location -Path "..\conductor_compiler"
    # emcmake cmake -S . -B build
    # if ($Jobs -gt 0) {
    #     cmake --build build -- "-j$Jobs"
    # }
    # else {
    #     cmake --build build
    # }

    # Set-Location -Path "..\score_compiler"
    # emcmake cmake -S . -B build
    # if ($Jobs -gt 0) {
    #     cmake --build build -- "-j$Jobs"
    # }
    # else {
    #     cmake --build build
    # }

    Set-Location -Path "..\frontend"
    Copy-Item "..\patch_compiler\build\math\math.wasm" "public\"

    Copy-Item "..\patch_compiler\build\app\wasmwasm.wasm" "src\wasmwasm\"
    Copy-Item "..\patch_compiler\build\app\wasmwasm.js" "src\wasmwasm\"

    # Copy-Item "..\conductor_compiler\build\app\conductorwasm.wasm" "src\conductorwasm\"
    # Copy-Item "..\conductor_compiler\build\app\conductorwasm.js" "src\conductorwasm\"

    # Copy-Item "..\score_compiler\build\app\scorewasm.wasm" "src\scorewasm\"
    # Copy-Item "..\score_compiler\build\app\scorewasm.js" "src\scorewasm\"

    Write-Host "Build complete and files copied."
    Set-Location -Path "..\"
}
catch {
    Write-Host "Error: $_"
    exit 1
}
