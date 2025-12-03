#!/bin/bash
set -u

# Пересобираем компилятор
make clean >/dev/null 2>&1
make all >/dev/null 2>&1

# Создаём директории для выходных файлов
mkdir -p build/outputs/wasm_wat build/outputs/wasm_bin

echo "Running all test cases"
echo ""
echo "------------------------------------------------"
echo ""

shopt -s nullglob
for test in tests/*.pi; do
  base=$(basename "$test" .pi)
  
  echo ">> $base"
  echo "-------------------------------------"
  
  # Компилируем в WAT
  if ./build/wasm_compiler "$test" "build/outputs/wasm_wat/${base}.wat" >/dev/null 2>&1; then
    # Компилируем в WASM
    if command -v wat2wasm >/dev/null 2>&1; then
      if wat2wasm "build/outputs/wasm_wat/${base}.wat" -o "build/outputs/wasm_bin/${base}.wasm" >/dev/null 2>&1; then
        # Запускаем
        if command -v wasmtime >/dev/null 2>&1; then
          if output=$(wasmtime "build/outputs/wasm_bin/${base}.wasm" 2>&1); then
            echo "$output"
          else
            echo "[Runtime error]"
            echo "$output"
          fi
        else
          echo "[wasmtime not installed]"
        fi
      else
        echo "[wat2wasm failed]"
      fi
    else
      echo "[wat2wasm not installed]"
    fi
  else
    echo "[Compilation failed]"
  fi
  
  echo ""
done

