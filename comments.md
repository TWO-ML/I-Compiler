## ЗАМЕЧАНИЕ 1
по документации к bool можно присваивать 0 или 1, а тут compilation error

// [RUNTIME ERROR] boolean := integer with invalid value
// Expect: runtime error (trap), since 2 is not 0/1

routine Main() is
  var b : boolean
  var x : integer is 2
  b := 1
  print b
end


(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $b i32)
      (local $__temp_check i32)
      i32.const 1
      local.tee $__temp_check
      i32.const 1
      i32.gt_u
      if
        then
        unreachable
        )
      local.get $__temp_check
      local.set $b
      local.get $b
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)

## ЗАМЕЧАНИЕ 2
неправильный вывод на этом тесте, исправь

// [OK] record + field access
// Expect output: 3 4

type Point is record
  var x : integer
  var y : integer
end

routine Main() is
  var p : Point
  p.x := 3
  p.y := 4
  print p.x, p.y
end

>> 010_record_field_access
-------------------------------------
0
0

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $p i32)
      (local $__temp_check i32)
      i32.const 2048
      local.set $p
      i32.const 3
      local.get $p
      i32.const 0
      i32.add
      i32.store
      i32.const 4
      local.get $p
      i32.const 4
      i32.add
      i32.store
      local.get $p
      i32.const 0
      i32.add
      i32.load
      call $print_i32
      local.get $p
      i32.const 4
      i32.add
      i32.load
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)


## ЗАМЕЧАНИЕ 3

тут неправильный вывод исправь чтобы правильно компилировало

// [OK] fixed-size array + for over range
// Expect output: 1 4 9 16 25

type Int5 is array[5] integer

routine Main() is
  var a : Int5
  var i : integer is 1
  while i <= 5 loop
    a[i] := i * i
    i := i + 1
  end
  for j in 1..5 loop
    print a[j]
  end
end

>> 011_array_fixed_and_for_range
-------------------------------------
0
2
2
0
3


(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $a i32)
      (local $i i32)
      (local $j i32)
      (local $start_j i32)
      (local $end_j i32)
      (local $arr_base_j i32)
      (local $arr_size_j i32)
      (local $idx_j i32)
      (local $__temp_check i32)
      i32.const 1
      local.set $i
      (block $end
        (loop $loop
          local.get $i
          i32.const 5
          i32.le_s
          i32.eqz
          br_if $end
          local.get $i
          local.get $i
          i32.mul
          local.get $a
          local.get $i
          i32.const 1
          i32.sub
          i32.add
          i32.store
          local.get $i
          i32.const 1
          i32.add
          local.set $i
          br $loop
        )
      )
      i32.const 1
      local.set $start_j
      i32.const 5
      local.set $end_j
      local.get $start_j
      local.set $j
      (block $end
        (loop $loop
          local.get $j
          local.get $end_j
          i32.gt_s
          br_if $end
          local.get $a
          local.get $j
          i32.const 1
          i32.sub
          i32.const 4
          i32.mul
          i32.add
          i32.load
          call $print_i32
          local.get $j
          i32.const 1
          i32.add
          local.set $j
          br $loop
        )
      )
      return
    )
  (func (export "_start")
    call $Main
  )
)


## ЗАМЕЧАНИЕ 4

почему не запустилсоь, исправь
// [OK] for v in array (iteration over elements)
// Expect output: 2 4 6

type Int3 is array[3] integer

routine Main() is
  var a : Int3
  a[1] := 2
  a[2] := 4
  a[3] := 6
  for v in a loop
    print v
  end
end

>> 012_for_iterate_over_array
-------------------------------------
[Runtime error]
Error: failed to run main module `build/outputs/wasm_bin/012_for_iterate_over_array.wasm`

Caused by:
    0: failed to invoke command default
    1: error while executing at wasm backtrace:
           0:     0xfd - <unknown>!<wasm function 3>
           1:    0x131 - <unknown>!<wasm function 4>
    2: memory fault at wasm address 0xfffffffc in linear memory of size 0x10000
    3: wasm trap: out of bounds memory access
    
    
    (module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $a i32)
      (local $v i32)
      (local $start_v i32)
      (local $end_v i32)
      (local $arr_base_v i32)
      (local $arr_size_v i32)
      (local $idx_v i32)
      (local $__temp_check i32)
      i32.const 2
      local.get $a
      i32.const 1
      i32.const 1
      i32.sub
      i32.add
      i32.store
      i32.const 4
      local.get $a
      i32.const 2
      i32.const 1
      i32.sub
      i32.add
      i32.store
      i32.const 6
      local.get $a
      i32.const 3
      i32.const 1
      i32.sub
      i32.add
      i32.store
      local.get $a
      local.set $arr_base_v
      local.get $arr_base_v
      i32.const 4
      i32.sub
      i32.load
      local.set $arr_size_v
      i32.const 0
      local.set $idx_v
      (block $end
        (loop $loop
          local.get $idx_v
          local.get $arr_size_v
          i32.ge_u
          br_if $end
          local.get $arr_base_v
          local.get $idx_v
          i32.const 4
          i32.mul
          i32.add
          i32.load
          local.set $v
          local.get $v
          call $print_i32
          local.get $idx_v
          i32.const 1
          i32.add
          local.set $idx_v
          br $loop
        )
      )
      return
    )
  (func (export "_start")
    call $Main
  )
)



## ЗАМЕЧАНИЕ 5

почему выводит букву Т, должно выводить число\
// [OK] function with return + call
// Expect output: 36

routine square(n : integer) : integer => n * n

routine Main() is
  print square(6)
end

>> 015_function_return_and_call
-------------------------------------
T


(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $square
      (param $n i32)
      (result i32)
      (local $__temp_check i32)
      local.get $n
      local.get $n
      i32.mul
    )
    (func $Main
      (local $__temp_check i32)
      i32.const 6
      call $square
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)



## ЗАМЕЧАНИЕ 6
тут Compilation error, хотя не должно быть, у меня по языку передача аргумента в функцию равняется присваиванию 


routine inc(n : integer) : integer => n + 1

routine Main() is
  var r : real is 3.14
  print inc(r)    
end

>> 016_diff_arg_type_in_call
-------------------------------------
[Compilation error]


(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $inc
      (param $n i32)
      (result i32)
      (local $__temp_check i32)
      local.get $n
      i32.const 1
      i32.add
    )
    (func $Main
      (local $r f64)
      (local $__temp_check i32)
      f64.const 3.140000
      local.set $r
      local.get $r
      call $inc
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)

Задача: исправить проверку типов и неявные преобразования в моём компиляторе/интерпретаторе языка (Imperative Language). Нужно корректно обрабатывать присваивание и передачу параметров по правилам спецификации.

Ключ из спецификации (важно соблюдать):
	1.	Передача аргументов в подпрограммы имеет ту же семантику, что и обычное присваивание (то есть те же правила конформности типов).  ￼
	2.	Таблица конверсий для примитивов при Assignment (левая часть :=, правая часть — выражение):

	•	integer := integer — копирование значения.  ￼
	•	integer := real — разрешено, округление к ближайшему целому.  ￼
	•	integer := boolean — разрешено: true → 1, false → 0.  ￼
	•	real := real — копирование значения.  ￼
	•	real := integer — разрешено, копирование (целое → вещественное).  ￼
	•	real := boolean — запрещено.  ￼
	•	boolean := boolean — копирование значения.  ￼
	•	boolean := integer — условно разрешено: если rhs ∈ {0,1} → false/true, иначе ошибка.  ￼
	•	boolean := real — запрещено.  ￼

	3.	Для пользовательских типов (record/array) — требуется полная идентичность типа, т.е. одна и та же объявленная user-type; они — reference types (присваивание копирует ссылку).  ￼


## ЗАМЕЧАНИЕ 7

тут какие-то проблемы с выводом\
// [OK] scope + shadowing
// Expect output: 1 2 1

routine Main() is
  var x : integer is 1
  print x         // 1
  if true then
    var x : integer is 2   // shadows outer x within the block
    print x       // 2
  end
  print x         // outer again: 1
end

>> 019_scope_and_shadowing
-------------------------------------
0
2
2

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $x i32)
      (local $x_1 i32)
      (local $__temp_check i32)
      i32.const 1
      local.set $x
      local.get $x_1
      call $print_i32
      i32.const 2
      local.set $x_1
      local.get $x_1
      call $print_i32
      local.get $x_1
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)


## ЗАМЕЧАНИЕ 8
почему он сбилдил wat и wasm файл если синтаксическая ошибка
/*
var x : integer



 Syntax errors:
  Syntax error at 1:1: expected declaration (var/type/routine) (got Error "unterminated block comment")

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

)

>> 025_bad_comment
-------------------------------------


## ЗАМЕЧАНИЕ 9
почему compilation error, должно работать - исправь
routine ping() is
  print 42
end

routine sum(a : integer, b : integer) : integer => a + b

routine Main() is
  ping()
  print sum(3, 5), sum(10, 20)
end
>> 036_calls_no_args_multi_args
-------------------------------------
[Compilation error]

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $ping
      (local $__temp_check i32)
      i32.const 42
      call $print_i32
      return
    )
    (func $sum
      (param $a i32)
      (param $b i32)
      (result i32)
      (local $__temp_check i32)
      local.get $a
      local.get $b
      i32.add
    )
    (func $Main
      (local $__temp_check i32)
      call $ping
      drop
      i32.const 3
      i32.const 5
      call $sum
      call $print_i32
      i32.const 10
      i32.const 20
      call $sum
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)


## ЗАМЕЧАНИЕ 10
работает неправильно, исправь

type Int5 is array[5] integer

routine square(n : integer) : integer => n * n

routine Main() is
  var xs : Int5
  var i : integer is 1
  while i <= 5 loop
    xs[i] := square(i)
    i := i + 1
  end
  for j in 1..5 reverse loop
    print xs[j]
  end
end

>> 040_demoB
-------------------------------------
3
0
2
2
0

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $square
      (param $n i32)
      (result i32)
      (local $__temp_check i32)
      local.get $n
      local.get $n
      i32.mul
    )
    (func $Main
      (local $xs i32)
      (local $i i32)
      (local $j i32)
      (local $start_j i32)
      (local $end_j i32)
      (local $arr_base_j i32)
      (local $arr_size_j i32)
      (local $idx_j i32)
      (local $__temp_check i32)
      i32.const 1
      local.set $i
      (block $end
        (loop $loop
          local.get $i
          i32.const 5
          i32.le_s
          i32.eqz
          br_if $end
          local.get $i
          call $square
          local.get $xs
          local.get $i
          i32.const 1
          i32.sub
          i32.add
          i32.store
          local.get $i
          i32.const 1
          i32.add
          local.set $i
          br $loop
        )
      )
      i32.const 1
      local.set $start_j
      i32.const 5
      local.set $end_j
      local.get $end_j
      local.set $j
      (block $end
        (loop $loop
          local.get $j
          local.get $start_j
          i32.lt_s
          br_if $end
          local.get $xs
          local.get $j
          i32.const 1
          i32.sub
          i32.const 4
          i32.mul
          i32.add
          i32.load
          call $print_i32
          local.get $j
          i32.const 1
          i32.sub
          local.set $j
          br $loop
        )
      )
      return
    )
  (func (export "_start")
    call $Main
  )
)

## ЗАМЕЧАНИЕ 11

некорректно работает, исправь
routine Main() is
  var a : integer is 5 + 3         // should become 8
  var b : integer is 10 * 2        // should become 20
  var c : integer is 15 - 7        // should become 8
  var d : boolean is 3 < 5         //  should become true
  var e : boolean is true and false //  should become false
  
  print a, b, c, d, e
end

>> semantic_001_constant_folding
-------------------------------------
8
D
8
1
0

(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $a i32)
      (local $b i32)
      (local $c i32)
      (local $d i32)
      (local $e i32)
      (local $__temp_check i32)
      i32.const 8
      local.set $a
      i32.const 20
      local.set $b
      i32.const 8
      local.set $c
      i32.const 1
      local.set $d
      i32.const 0
      local.set $e
      local.get $a
      call $print_i32
      local.get $b
      call $print_i32
      local.get $c
      call $print_i32
      local.get $d
      call $print_i32
      local.get $e
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)

## ЗАМЕЧАНИЕ 12

не должно быть ошитбки, должно округлиться до 3 и передаться в функцию


routine square(n : integer) : integer => n * n

routine Main() is
  var r : real is 3.14
  print square(r) 
end


>> semantic_009_type_checking_args
-------------------------------------
[Compilation error]
(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 1024))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.store8
      (local.get $buf)
      (i32.add (i32.const 48) (local.get $value)))
    (i32.store8
      (i32.add (local.get $buf) (i32.const 1))
      (i32.const 10))
    (i32.store (i32.const 0) (local.get $buf))
    (i32.store (i32.const 4) (i32.const 2))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    call $fd_write
    drop
  )

  (func $print_f64 (param $value f64)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $square
      (param $n i32)
      (result i32)
      (local $__temp_check i32)
      local.get $n
      local.get $n
      i32.mul
    )
    (func $Main
      (local $r f64)
      (local $__temp_check i32)
      f64.const 3.140000
      local.set $r
      local.get $r
      call $square
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)
