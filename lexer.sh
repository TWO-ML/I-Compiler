#!/bin/bash
# set -euo pipefail

# 1. Create directory for outputs
OUTDIR="build/outputs"
mkdir -p "$OUTDIR"
mkdir -p "$OUTDIR/compact"
mkdir -p "$OUTDIR/full"


# 2. Compile lexer
make

# 3. Run lexer on all tests
for test in tests/*.pi; do
    base=$(basename "$test" .pi)
    out="$OUTDIR/full/${base}.txt"
        compact="$OUTDIR/compact/${base}.compact.txt"


    echo "Running $base"
    # echo ""
    ./build/lexer "$test" > "$out"
    ./build/lexer "$test" | awk '{print $2}' | paste -sd' ' - > "$compact" 
    # ./build/lexer "$test" | awk '{print $2}' | paste -sd' ' -
    # echo ""

done