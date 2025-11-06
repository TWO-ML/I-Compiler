# Дополнительные проверки типов

## Реализованные проверки

### 1. Строгая проверка типов присваивания и return ✓

**Описание:** Проверяет строгое совпадение типов при присваивании и возврате значений из функций.

**Примеры:**

```pascal
// Ошибка: присваивание несовместимых типов
var a : integer is 5
var b : real is 3.14
a := b  // ERROR: type mismatch in assignment: left is 'integer', right is 'real'

// Ошибка: неправильный тип возврата
routine isPositive(n : integer) : boolean is
  return n  // ERROR: return type mismatch: expected 'boolean', got 'integer'
end
```

**Тесты:**
- `tests/semantic_007_type_checking_assign.pi` - проверка типов присваивания
- `tests/semantic_008_type_checking_return.pi` - проверка типов return

---

### 2. Проверка типов фактических аргументов ✓

**Описание:** Проверяет совпадение типов фактических аргументов с типами формальных параметров функций. Использует инференцию типов выражений.

**Пример:**

```pascal
routine square(n : integer) : integer => n * n

routine Main() is
  var r : real is 3.14
  print square(r)  // ERROR: type mismatch for argument 1: expected 'integer', got 'real'
end
```

**Тест:**
- `tests/semantic_009_type_checking_args.pi`

---

### 3. Предупреждение о выходе за границы массива ✓

**Описание:** Для массивов с константной длиной (например, `array[5] integer`) проверяет константные индексы на соответствие границам 1..N. Выдаёт предупреждение, не прерывая компиляцию.

**Пример:**

```pascal
type Int5 is array[5] integer

routine Main() is
  var arr : Int5
  arr[3] := 10   // OK: индекс в границах 1..5
  arr[6] := 20   // WARNING: index 6 is out of bounds 1..5
  arr[0] := 30   // WARNING: index 0 is out of bounds 1..5
end
```

**Тест:**
- `tests/semantic_010_array_bounds.pi`

---

## Реализация

### Новые структуры данных

```cpp
// Описание типа
struct TypeInfo {
    string kind = "unknown";  // "integer" | "real" | "boolean" | "array" | имя типа
    int arrayLen = -1;        // для массивов: длина (-1 = неизвестно)
    string elemKind = "";     // для массивов: тип элемента
};

// Таблицы типов
unordered_map<string, TypeInfo> typeDefs;              // именованные типы
unordered_map<string, vector<TypeInfo>> funcParamTypesTI;  // типы параметров функций
unordered_map<string, TypeInfo> funcReturnTI;          // типы возврата функций
vector<string> routineNameStack;                       // стек имён текущих функций
```

### Ключевые функции

1. **`typeFromTypeNode(AST* t)`** - парсинг типа из AST узла
2. **`buildTypeTable(AST* n)`** - построение таблицы именованных типов
3. **`inferExprType(AST* e)`** - инференция типа выражения
4. **`typesEqual(const TypeInfo& a, const TypeInfo& b)`** - сравнение типов

### Места проверок

- **Assign** - проверка `type(LHS) == type(RHS)`
- **ReturnStmt** - проверка `type(expr) == returnType(function)`
- **Call** - проверка `type(arg[i]) == type(param[i])`
- **ModPrimary::Index** - проверка `1 <= constIndex <= arrayLen`

---

## Примеры использования

### Запуск отдельных тестов

```bash
# Проверка типов присваивания
./build/semantic_analyzer tests/semantic_007_type_checking_assign.pi

# Проверка типов return
./build/semantic_analyzer tests/semantic_008_type_checking_return.pi

# Проверка типов аргументов
./build/semantic_analyzer tests/semantic_009_type_checking_args.pi

# Проверка границ массива
./build/semantic_analyzer tests/semantic_010_array_bounds.pi

# Комплексный тест
./build/semantic_analyzer tests/semantic_011_all_type_checks.pi
```

---

## Совместимость

✅ Все старые тесты продолжают работать  
✅ Логи и выводы не изменены  
✅ Оптимизации работают как раньше  
✅ Добавлены только новые проверки  

---

## Статистика

**Проверок (не модифицируют AST):** 7  
- 4 старые + 3 новые

**Оптимизаций (модифицируют AST):** 4  

**Всего возможностей:** 11

---

**Реализовано:** ноябрь 2024  
**Innopolis University, Compiler Construction**

