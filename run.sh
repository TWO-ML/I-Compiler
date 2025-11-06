#!/bin/bash
set -euo pipefail

OUTDIR="build/outputs"
TOK_FULL="$OUTDIR/tokens/full"
TOK_COMPACT="$OUTDIR/tokens/compact"
PARSE_OUT="$OUTDIR/parse"
ANALYZER_OUT="$OUTDIR/analyze"

mkdir -p "$TOK_FULL" "$TOK_COMPACT" "$PARSE_OUT" "$ANALYZER_OUT"

make -q build/lexer 2>/dev/null || make build/lexer
make -q build/parser 2>/dev/null || make build/parser
make -q build/semantic_analyzer 2>/dev/null || make build/semantic_analyzer


shopt -s nullglob
for test in tests/*.pi; do
  base="$(basename "$test" .pi)"
  echo "==> $base"

  ./build/lexer "$test" > "$TOK_FULL/${base}.txt"

  ./build/lexer "$test" \
    | awk '{print $2}' \
    | paste -sd' ' - \
    > "$TOK_COMPACT/${base}.compact.txt"

  ./build/parser "$test" > "$PARSE_OUT/${base}.parse.txt" 2>&1 || true
  ./build/semantic_analyzer "$test" > "$ANALYZER_OUT/${base}.analyze.txt" 2>&1 || true

done