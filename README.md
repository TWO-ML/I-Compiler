# I-Compiler

Компилятор для языка Project I с лексическим, синтаксическим и семантическим анализом.

## Компоненты

### 1. Лексический анализатор (Lexer)
```bash
make build/lexer
./build/lexer tests/001_expressions.pi
```

### 2. Синтаксический анализатор (Parser)
```bash
make build/parser
./build/parser tests/001_expressions.pi
```

### 3. Семантический анализатор (Semantic Analyzer)
```bash
make build/semantic_analyzer
./build/semantic_analyzer tests/001_expressions.pi
```

### 4. Компилятор в WebAssembly (WASM Compiler)
```bash
make build/wasm_compiler
./build/wasm_compiler tests/001_expressions.pi output.wat
```

## Быстрый старт

### Сборка всего проекта
```bash
make all
```

Это соберёт все компоненты, включая компилятор в WebAssembly.

### Полный запуск тестов из папки /demo_tests
```bash
chmod +x run.sh
./run.sh
```
### Запуск Wasm файлов для получения атупута
### З
```bash
chmod +x run_all_wasm.sh
./run_all_wasm.sh
```

pi` - все оптимизации

## Структура проекта

```
I-Compiler/
├── src/
│   ├── lexer.cpp               # Лексический анализатор
│   ├── parser.cpp              # Синтаксический анализатор
│   ├── semantic_analyzer.cpp   # Семантический анализатор
│   └── wasm_compiler.cpp        # Компилятор в WebAssembly
├── tests/                      # Тестовые файлы
├── build/                      # Собранные исполняемые файлы
├── Makefile
├── run.sh       # Демонстрация выполнения компилятора
├── run_all_wasm.sh            # Скрипт для запуска WASM
├── README.md                   # Этот файл    
└── description.pdf             # Документация по языку
```

## Команды Make

```bash
make all                        # Собрать все компоненты
make build/lexer               # Собрать только лексер
make build/parser              # Собрать только парсер
make build/semantic_analyzer   # Собрать только семантический анализатор
make build/wasm_compiler       # Собрать только компилятор WASM
make clean                     # Удалить собранные файлы
```

## Компилятор в WebAssembly

Компилятор преобразует исходный код на языке Project I в WebAssembly Text (WAT) формат, который затем может быть скомпилирован в бинарный WASM файл.

### Использование

```bash
# Компиляция в WAT
./build/wasm_compiler tests/001_expressions.pi output.wat

# Использование скрипта (автоматически конвертирует в WASM, если установлен wat2wasm)
./run_wasm_test.sh tests/001_expressions.pi

# Запуск скомпилированного WASM файла
wasmtime output.was
# или
wasmer output.was
```

### Требования

Для конвертации WAT в WASM и запуска:
- `wat2wasm` (из пакета wabt): `brew install wabt` или `apt-get install wabt`
- `wasmtime` или `wasmer` для запуска WASM файлов

### Поддерживаемые конструкции

- ✅ Переменные (integer, real, boolean)
- ✅ Функции (routine)
- ✅ Условные операторы (if-then-else)
- ✅ Циклы (while, for)
- ✅ Выражения (арифметические, логические, сравнения)
- ✅ Вызовы функций
- ✅ Print (базовая поддержка через WASI)
- ⚠️ Массивы (частичная поддержка)
- ⚠️ Records (частичная поддержка)


**Innopolis University, 2025**
**Compiler Construction Course**