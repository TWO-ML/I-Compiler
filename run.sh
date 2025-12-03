#!/bin/bash
set -euo pipefail

OUTDIR="build/outputs"
TOK_FULL="$OUTDIR/tokens/full"
TOK_COMPACT="$OUTDIR/tokens/compact"
PARSE_OUT="$OUTDIR/parse"
ANALYZER_OUT="$OUTDIR/analyze"
WASM_WAT="$OUTDIR/wasm_wat"
WASM_BIN="$OUTDIR/wasm_bin"

rm -rf build/
mkdir -p "$TOK_FULL" "$TOK_COMPACT" "$PARSE_OUT" "$ANALYZER_OUT" "$WASM_WAT" "$WASM_BIN"

make -q build/lexer             2>/dev/null || make build/lexer
make -q build/parser            2>/dev/null || make build/parser
make -q build/semantic_analyzer 2>/dev/null || make build/semantic_analyzer
make -q build/wasm_compiler     2>/dev/null || make build/wasm_compiler

shopt -s nullglob
for test in demo_tests/*.pi; do
  base="$(basename "$test" .pi)"
  echo "==> $base"

  if ! ./build/lexer "$test" > "$TOK_FULL/${base}.txt"; then
    echo "  [lexer] failed, skipping $base"
    continue
  fi

  ./build/lexer "$test" \
    | awk '{print $2}' \
    | paste -sd' ' - \
    > "$TOK_COMPACT/${base}.compact.txt" || true

  ./build/parser "$test" > "$PARSE_OUT/${base}.parse.txt" 2>&1 || true
  ./build/semantic_analyzer "$test" > "$ANALYZER_OUT/${base}.analyze.txt" 2>&1 || true

  if ! ./build/wasm_compiler "$test" "$WASM_WAT/${base}.wat"; then
    echo "  [wasm_compiler] failed, skipping WASM for $base"
    continue
  fi

  if command -v wat2wasm >/dev/null 2>&1; then
    if ! wat2wasm "$WASM_WAT/${base}.wat" -o "$WASM_BIN/${base}.wasm"; then
      echo "  [wat2wasm] failed to assemble $base.wat"
    fi
  fi
done
