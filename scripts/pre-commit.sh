#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"

cd "$ROOT"

# STAGED_CPP=$(git ls-files | grep -E '\.(cpp|hpp|h|c)$' || true)
STAGED_CPP=$(git diff --cached --name-only --diff-filter=ACMR | { grep -E '\.(cpp|hpp|h|c)$' || true; })

if [ -n "$STAGED_CPP" ]; then
	while IFS= read -r file; do
		before=$(git show :"$file" 2>/dev/null | md5sum)
		if ! clang-format -i --style=file "$ROOT/$file"; then
			echo "  clang-format failed: $file"
			exit 1
		fi
		after=$(md5sum <"$ROOT/$file")
		if [ "$before" != "$after" ]; then
			echo "  clang-format: $file"
		fi
		git add "$file"
	done < <(printf '%s\n' "$STAGED_CPP")
fi

# STAGED_FRONTEND=$(git ls-files | grep -E '^frontend/.*\.(ts|html|scss|css|json)$' || true)
STAGED_FRONTEND=$(git diff --cached --name-only --diff-filter=ACMR | { grep -E '^frontend/.*\.(tsx|ts|html|scss|css|json)$' || true; })

if [ -n "$STAGED_FRONTEND" ]; then
	FRONTEND_DIR="$ROOT/frontend"
	FRONTEND_FILES=$(printf '%s\n' "$STAGED_FRONTEND" | sed "s|^|$ROOT/|")

	if ! (cd "$FRONTEND_DIR" && echo "$FRONTEND_FILES" | xargs npx prettier --write) 2>&1; then
		echo "  prettier: formatting failed"
		exit 1
	fi

	while IFS= read -r file; do
		git add "$ROOT/$file"
	done < <(printf '%s\n' "$STAGED_FRONTEND")
fi
