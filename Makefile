CXX := clang++              # or g++
CXXFLAGS := -std=c++20 -O2 -Wall -Wextra -pedantic

build/lexer: src/lexer.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

build/parser: src/parser.cpp
	$(CXX) $(CXXFLAGS) src/parser.cpp -o $@

build/semantic_analyzer: src/semantic_analyzer.cpp
	$(CXX) $(CXXFLAGS) src/semantic_analyzer.cpp -o $@

build/wasm_compiler: src/wasm_compiler.cpp
	$(CXX) $(CXXFLAGS) src/wasm_compiler.cpp -o $@

all: build/lexer build/parser build/semantic_analyzer build/wasm_compiler

clean:
	rm -f build/lexer build/parser build/semantic_analyzer build/wasm_compiler