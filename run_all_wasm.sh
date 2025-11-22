#!/bin/bash
set -u  # без -e, чтобы не падать на ошибках

WASM_DIR="build/outputs/wasm_bin"

if ! command -v wasmtime >/dev/null 2>&1; then
  echo "Error: wasmtime not installed!"
  exit 1
fi

shopt -s nullglob
FILES=("$WASM_DIR"/*.wasm)

if [ ${#FILES[@]} -eq 0 ]; then
  echo "No WASM files found in $WASM_DIR"
  exit 0
fi

echo "Running all WebAssembly binaries from $WASM_DIR"
echo "------------------------------------------------"

for wasm in "${FILES[@]}"; do
  base=$(basename "$wasm")

  echo ">> Running $base"
  echo "-------------------------------------"

  # Запускаем и не падаем при ошибке
  if output=$(wasmtime "$wasm" 2>&1); then
    echo "$output"
  else
    echo "[Runtime error]"
    echo "$output"
  fi

  echo
done
