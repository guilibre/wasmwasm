# Usage: .\run.ps1 [source.ww]
#        echo "OUT[0] <- sin(TIME)" | .\run.ps1

$BuildDir = Join-Path $PSScriptRoot "..\build\debugger"
$Script = Join-Path $BuildDir "ww_debug.js"

if ($args.Count -gt 0) {
    $SourcePath = Resolve-Path $args[0]
    $Src = Get-Content $SourcePath -Raw
} else {
    $Src = $input -join "`n"
}

Push-Location $BuildDir
try {
    $Src | node --experimental-wasm-exnref $Script
} finally {
    Pop-Location
}
