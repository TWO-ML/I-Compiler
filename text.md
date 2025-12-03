

Slide 1 — Project Title

WASM Compiler for Language I

Team ML
Leonid Merkulov, Lavrova Marina

⸻

Slide 2 — Task Description

Our project is a working compiler for Language I, a small imperative programming language designed for educational purposes.
The goal of the project was to:
	•	implement a full compilation pipeline: lexing → parsing → semantic analysis → code generation;
	•	support the complete Language-I grammar (variables, records, arrays, loops, functions, expressions);
	•	compile source programs written in Language I into WebAssembly (WASM) so they can be executed in any WASM runtime.

Source language: Language I
Implementation language: C++
Parser development tool: Hand-written recursive-descent parser
Target platform: WebAssembly (WASM)

The expected result was a compiler capable of producing correct WASM output for non-trivial programs, including arithmetic operations, nested scopes, loops, arrays, functions, and record types.

⸻

Slide 3 — Overall Architecture

We implemented the classical architecture of a modern compiler:
	1.	Lexer
Converts the input character stream into a sequence of tokens: identifiers, numbers, operators, keywords.
	2.	Parser (hand-written)
Builds the Abstract Syntax Tree (AST) according to the grammar of Language I.
The parser is recursive-descent, fully handwritten without Bison/ANTLR.
	3.	Semantic Analyzer
Performs:
	•	scope checking
	•	type checking
	•	validation of records, arrays, routine signatures
	•	detection of undefined identifiers
	•	checking reference-type assignment rules (arrays/records)
	4.	WASM Code Generator
Generates instructions in WebAssembly text format (.wat), including:
	•	locals and globals
	•	control-flow constructs (if, block, loop)
	•	numeric operations
	•	load/store operations for arrays and records
	•	function bodies and return statements

⸻

Slide 4 — Technology
	•	Lexer: custom state-machine with category recognition
	•	Parser: handwritten LL(1)/recursive-descent
	•	Intermediate representation: AST structures in C++
	•	Semantic checking: symbol tables with nested scopes
	•	Code generation: direct AST-to-WASM translation
	•	stack-based evaluation for expressions
	•	memory addressing for arrays and records
	•	WASM local.get, local.set, i32.add, i32.store, i32.load, etc.

⸻

Slide 5 — Major Data Structures
	1.	Tokens
	•	keyword, identifier, integer_literal, real_literal, boolean_literal
	•	operators: + - * / % < > <= >= = /= and or xor
	•	separators: ; , ( ) [ ] .
	2.	AST Nodes
	•	Expressions (binary, unary, literal, variable reference, call)
	•	Statements (assignment, while, for, if, print, routine call)
	•	Declarations (var, type, routine)
	•	Types: primitive, array, record
	3.	Symbol Tables
	•	nested scopes
	•	binding identifiers to types
	•	detection of shadowing
	4.	WASM Code Model
	•	list of functions
	•	memory layout for aggregates
	•	offset tables for record fields

⸻

Slide 6 — Results

✔ What is done
	•	full lexer
	•	full recursive-descent parser
	•	complete AST
	•	semantic analyzer covering:
	•	variables, types, records, arrays
	•	routines and parameters
	•	assignment compatibility and reference types
	•	WASM generator with:
	•	arithmetic
	•	boolean logic
	•	arrays + record field offsets
	•	loops (while/for)
	•	if/else
	•	procedure calls and returns
	•	demo programs and example tests

❗ What is not fully finished

	•	garbage collection for user-defined types
	•	full WASM memory bounds checking
	•	runtime library for printing reals
	•	error recovery after parsing failures

⸻

Slide 7 — Team Contribution

Leonid Merkulov
	•	Lexer implementation
	•	Core of the recursive descent parser
	•	Design of the AST
	•	WASM code generation for expressions
	•	Main integration of the compiler pipeline

Lavrova Marina
	•	Complete semantic analyzer and scope system
	•	Support for records and arrays
	•	WASM generation for statements
	•	For-loop, while-loop, if-statement generation
	•	Test programs & debugging
	•	Documentation and presentation

⸻

Slide 8 — Code Repository

GitHub link:

