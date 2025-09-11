CXX := clang++              # or g++
CXXFLAGS := -std=c++17 -O2 -Wall -Wextra -pedantic

build/lexer: src/lexer.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -f lexer