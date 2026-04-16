#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"

cd "$ROOT"

STAGED_FRONTEND=$(git ls-files | grep -E '^frontend/.*\.(ts|html|scss|css|json)$' || true)

if [ -n "$STAGED_FRONTEND" ]; then
	FRONTEND_DIR="$ROOT/frontend"

	ESLINT_FILES=$(printf '%s\n' "$STAGED_FRONTEND" | grep -E '\.(ts|html)$' | sed "s|^|$ROOT/|" || true)
	if [ -z "$ESLINT_FILES" ]; then exit 0; fi
	if ! (cd "$FRONTEND_DIR" && echo "$ESLINT_FILES" | xargs npx eslint) 2>&1; then
		echo "  eslint: fix the warnings above before committing"
		exit 1
	fi
fi

STAGED_CPP=$(git ls-files | grep -E '\.(cpp|hpp|h|c)$' || true)

# clang-tidy reads compile_commands.json directly, bypassing .clangd Remove filters.
# Strip flags that are only valid for the emscripten/modules build toolchain.
filtered_build_dir() {
	local build_dir="$1"
	local tmp_dir
	tmp_dir="$(mktemp -d)"
	sed -E 's/ -f(deps|module)[^ "]*//g; s/ -s[A-Z][^ "]*//g; s/ -lembind//g' \
		"$build_dir/compile_commands.json" >"$tmp_dir/compile_commands.json"
	echo "$tmp_dir"
}

if [ -n "$STAGED_CPP" ]; then
	TIDY_FAILED=0
	while IFS= read -r file; do
		build_dir="$ROOT/compiler/build" || {
			TIDY_FAILED=1
			continue
		}
		tmp_dir="$(filtered_build_dir "$build_dir")"
		if ! clang-tidy -p "$tmp_dir" "$ROOT/$file" 2>&1; then
			TIDY_FAILED=1
		fi
		rm -rf "$tmp_dir"
	done < <(printf '%s\n' "$STAGED_CPP")
	if [ "$TIDY_FAILED" -ne 0 ]; then
		echo "  clang-tidy: fix the warnings above before committing"
		exit 1
	fi
fi
