#!/bin/bash
set -u 

WASM_DIR="build/outputs/wasm_bin"
WAT_DIR="build/outputs/wasm_wat"
if ! command -v wasmtime >/dev/null 2>&1; then
  echo "Error: wasmtime not installed!"
  exit 1
fi

echo "Running all test cases"
echo "------------------------------------------------"

shopt -s nullglob
for wat in "$WAT_DIR"/*.wat; do
  wat_base=$(basename "$wat")
  name="${wat_base%.wat}"
  wasm="$WASM_DIR/${name}.wasm"

  echo ">> $name"
  echo "-------------------------------------"

  if [ ! -e "$wasm" ]; then
    echo "[Compilation error]"
    echo
    continue
  fi

  if output=$(wasmtime "$wasm" 2>&1); then
    echo "$output"
  else
    echo "[Runtime error]"
    echo "$output"
  fi

  echo
done