#!/bin/bash
# Скрипт для демонстрации работы семантического анализатора

echo "=========================================="
echo "  Семантический Анализатор для Project I"
echo "=========================================="
echo ""

# Цвета для вывода
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Функция для красивого вывода заголовка
print_header() {
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
}

# Проверка, что анализатор собран
if [ ! -f "./build/semantic_analyzer" ]; then
    echo -e "${RED}Ошибка: semantic_analyzer не найден. Сначала выполните: make build/semantic_analyzer${NC}"
    exit 1
fi

# ПРОВЕРКИ (НЕ МОДИФИЦИРУЮТ AST)
print_header "ПРОВЕРКА 1: Использование переменной до объявления"
echo "Файл: tests/006_use_before_declaration.pi"
echo ""
./build/semantic_analyzer tests/006_use_before_declaration.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

print_header "ПРОВЕРКА 2: return вне функции"
echo "Файл: tests/semantic_005_return_outside_routine.pi"
echo ""
./build/semantic_analyzer tests/semantic_005_return_outside_routine.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

print_header "ПРОВЕРКА 3: Корректный код (все проверки пройдены)"
echo "Файл: tests/015_function_return_and_call.pi"
echo ""
./build/semantic_analyzer tests/015_function_return_and_call.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

# ОПТИМИЗАЦИИ (МОДИФИЦИРУЮТ AST)
print_header "ОПТИМИЗАЦИЯ 1: Свёртка констант"
echo "Файл: tests/semantic_001_constant_folding.pi"
echo "Демонстрация: 5 + 3 -> 8, 10 * 2 -> 20, 3 < 5 -> true, и т.д."
echo ""
./build/semantic_analyzer tests/semantic_001_constant_folding.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

print_header "ОПТИМИЗАЦИЯ 2: Удаление недостижимого кода"
echo "Файл: tests/semantic_002_unreachable_code.pi"
echo "Демонстрация: код после return будет удалён"
echo ""
./build/semantic_analyzer tests/semantic_002_unreachable_code.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

print_header "ОПТИМИЗАЦИЯ 3: Упрощение if"
echo "Файл: tests/semantic_003_if_simplification.pi"
echo "Демонстрация: if (true) -> тело then, if (false) -> тело else"
echo ""
./build/semantic_analyzer tests/semantic_003_if_simplification.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

print_header "ОПТИМИЗАЦИЯ 4: Удаление неиспользуемых переменных"
echo "Файл: tests/semantic_004_unused_variables.pi"
echo "Демонстрация: переменные b и c не используются и будут удалены"
echo ""
./build/semantic_analyzer tests/semantic_004_unused_variables.pi
echo ""
read -p "Нажмите Enter для продолжения..."
echo ""

# КОМПЛЕКСНЫЙ ТЕСТ
print_header "КОМПЛЕКСНЫЙ ТЕСТ: Все оптимизации вместе"
echo "Файл: tests/semantic_006_all_optimizations.pi"
echo ""
./build/semantic_analyzer tests/semantic_006_all_optimizations.pi
echo ""

echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}  Демонстрация завершена!${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

