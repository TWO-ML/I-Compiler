#!/bin/bash

# Script to compile .pi file to WASM and run it

if [ $# -lt 1 ]; then
    echo "Usage: $0 <source.pi> [output.wat]"
    exit 1
fi

SOURCE_FILE="$1"
OUTPUT_WAT="${2:-output.wat}"
OUTPUT_WASM="${OUTPUT_WAT%.wat}.wasm"

# Build the compiler if needed
if [ ! -f build/wasm_compiler ]; then
    echo "Building wasm_compiler..."
    make build/wasm_compiler
    if [ $? -ne 0 ]; then
        echo "Failed to build wasm_compiler"
        exit 1
    fi
fi

# Compile to WAT
echo "Compiling $SOURCE_FILE to $OUTPUT_WAT..."
./build/wasm_compiler "$SOURCE_FILE" "$OUTPUT_WAT"

if [ $? -ne 0 ]; then
    echo "Compilation failed"
    exit 1
fi

echo "Generated WAT file: $OUTPUT_WAT"

# Check if wat2wasm is available
if command -v wat2wasm &> /dev/null; then
    echo "Converting WAT to WASM..."
    wat2wasm "$OUTPUT_WAT" -o "$OUTPUT_WASM"
    
    if [ $? -eq 0 ]; then
        echo "Generated WASM file: $OUTPUT_WASM"
        echo ""
        echo "To run the WASM file, use:"
        echo "  wasmtime $OUTPUT_WASM"
        echo "or"
        echo "  wasmer $OUTPUT_WASM"
    else
        echo "Failed to convert WAT to WASM (wat2wasm not found or error)"
    fi
else
    echo "wat2wasm not found. Install wabt package to convert WAT to WASM"
    echo "  brew install wabt  # on macOS"
    echo "  apt-get install wabt  # on Ubuntu"
fi



