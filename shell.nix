{
  pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/d5dfd8e6716dde34398bc14bc87c10dece9c8c68.tar.gz";
    sha256 = "1gh4gz4z0nn15fnmzdhvdn9nngmfswxx4w9rjcr0i1ldzw2n886l";
  }) { },
}:

pkgs.mkShell {
  name = "wasmwasm";

  packages = with pkgs; [
    ccache
    clang-tools
    cmake
    direnv
    emscripten
    nodejs_24
    npm-check-updates
    patchelf
  ];

  shellHook = ''
    export CMAKE_POLICY_VERSION_MINIMUM=3.5

    if [ -d frontend/node_modules ]; then
      _nix_ld="$(cat "$(dirname "$(dirname "$(readlink -f "$(command -v cc)")")")/nix-support/dynamic-linker")"
      find frontend/node_modules -type f 2>/dev/null | while read -r f; do
        if file "$f" 2>/dev/null | grep -q "interpreter /lib64/ld-linux\|interpreter /lib/ld-linux"; then
          patchelf --set-interpreter "$_nix_ld" --set-rpath "$(patchelf --print-rpath "$f")" "$f" \
            && echo "patched for NixOS: $f"
        fi
      done
      unset _nix_ld
    fi

    echo "wasmwasm dev shell"
    echo "  emcc:   $(emcc --version | head -n1)"
    echo "  cmake:  $(cmake --version | head -n1)"
    echo "  node:   $(node --version)"
    echo "  ccache: $(ccache --version | head -n1)"
  '';
}
