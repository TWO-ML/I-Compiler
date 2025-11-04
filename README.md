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

### 3. Семантический анализатор (Semantic Analyzer) ✨ НОВОЕ
```bash
make build/semantic_analyzer
./build/semantic_analyzer tests/001_expressions.pi
```

## Быстрый старт

### Сборка всего проекта
```bash
make all
```

### Демонстрация семантического анализатора
```bash
chmod +x run_semantic_tests.sh
./run_semantic_tests.sh
```

## Семантический анализатор

### Проверки (не модифицируют AST)
1. ✓ Объявление переменных и функций до использования
2. ✓ Корректное использование `return` (только внутри функций)
3. ✓ Проверка типов аргументов функций

### Оптимизации (модифицируют AST)
1. ✓ Свёртка констант (`5 + 3` → `8`, `3 < 5` → `true`)
2. ✓ Удаление недостижимого кода (после `return`)
3. ✓ Упрощение условных конструкций (`if true` → тело then)
4. ✓ Удаление неиспользуемых переменных

### Документация
- [SEMANTIC_ANALYZER.md](SEMANTIC_ANALYZER.md) - полная документация
- [PRESENTATION.md](PRESENTATION.md) - материалы для презентации

## Тестовые файлы

### Базовые тесты
- `tests/001_expressions.pi` - выражения
- `tests/015_function_return_and_call.pi` - функции
- `tests/006_use_before_declaration.pi` - ошибка: использование до объявления

### Семантические тесты
- `tests/semantic_001_constant_folding.pi` - свёртка констант
- `tests/semantic_002_unreachable_code.pi` - недостижимый код
- `tests/semantic_003_if_simplification.pi` - упрощение if
- `tests/semantic_004_unused_variables.pi` - неиспользуемые переменные
- `tests/semantic_005_return_outside_routine.pi` - ошибка: return вне функции
- `tests/semantic_006_all_optimizations.pi` - все оптимизации

## Структура проекта

```
I-Compiler/
├── src/
│   ├── lexer.cpp               # Лексический анализатор
│   ├── parser.cpp              # Синтаксический анализатор
│   └── semantic_analyzer.cpp   # Семантический анализатор
├── tests/                      # Тестовые файлы
├── build/                      # Собранные исполняемые файлы
├── Makefile
├── run_semantic_tests.sh       # Демонстрация семантического анализатора
├── README.md                   # Этот файл
├── SEMANTIC_ANALYZER.md        # Документация семантического анализатора
└── PRESENTATION.md             # Материалы для презентации
```

## Команды Make

```bash
make all                        # Собрать все компоненты
make build/lexer               # Собрать только лексер
make build/parser              # Собрать только парсер
make build/semantic_analyzer   # Собрать только семантический анализатор
make clean                     # Удалить собранные файлы
```

## Презентация (6 ноября 2024)

Для демонстрации на лабораторной работе:
```bash
./run_semantic_tests.sh
```

Или вручную:
```bash
./build/semantic_analyzer tests/semantic_006_all_optimizations.pi
```

---

**Innopolis University, 2025**
**Compiler Construction Course**