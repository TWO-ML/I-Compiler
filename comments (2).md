исправь все эти замечания и самое главное - исправь, чтобы цифры не превращались в буквы после 10, ощущение, что там какая-то 16тиричная система вычисления или что-то еще. Но все цифры больше 10 в выводже должны оставаться цифрами

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

>> 008_assign_boolean_from_bad_integer_runtime_error
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

почему тут он выводит фигню 

>> 035_nested_var_decl
-------------------------------------
0
`
0

type Vec3  is array[3] real
type Point is record
  var x : integer
  var y : integer
end
type Box is record
  var p1 : Point
  var p2 : Point
  var data : array[2] Vec3
end

routine Main() is
  var b : Box
  b.p1.x := 1
  b.p2.y := 2
  b.data[1][3] := 3.14
  print(b.p1.x)
  print(b.p2.y)
  print(b.data[1][3])
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
      local.get $b
      i32.const 0
      i32.add
      local.get $__temp_check
      i32.store
      i32.const 2
      local.tee $__temp_check
      local.get $b
      i32.const 0
      i32.add
      local.get $__temp_check
      i32.store
      f64.const 3.140000
      f64.nearest
      i32.trunc_f64_s
      local.tee $__temp_check
      local.get $b
      i32.const 0
      i32.add
      local.get $__temp_check
      i32.store
      local.get $b
      i32.const 0
      i32.add
      i32.load
      i32.load
      call $print_i32
      local.get $b
      i32.const 0
      i32.add
      i32.load
      i32.load
      call $print_i32
      local.get $b
      i32.const 0
      i32.add
      i32.load
      i32.const 1
      i32.const 1
      i32.sub
      i32.const 4
      i32.mul
      i32.const 4
      i32.add
      i32.add
      i32.load
      i32.const 3
      i32.const 1
      i32.sub
      i32.const 4
      i32.mul
      i32.const 4
      i32.add
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

почему цифры превращаются в буквы, исправь или скажи почему так и как мне запускать иначе

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
1
2
9
@
I
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
          local.tee $__temp_check
          local.get $a
          local.get $i
          i32.const 1
          i32.sub
          i32.const 4
          i32.mul
          i32.const 4
          i32.add
          i32.add
          local.get $__temp_check
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
          i32.const 4
          i32.add
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

почему пустой аутпут, исправьт
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
      local.tee $__temp_check
      local.get $a
      i32.const 1
      i32.const 1
      i32.sub
      i32.const 4
      i32.mul
      i32.const 4
      i32.add
      i32.add
      local.get $__temp_check
      i32.store
      i32.const 4
      local.tee $__temp_check
      local.get $a
      i32.const 2
      i32.const 1
      i32.sub
      i32.const 4
      i32.mul
      i32.const 4
      i32.add
      i32.add
      local.get $__temp_check
      i32.store
      i32.const 6
      local.tee $__temp_check
      local.get $a
      i32.const 3
      i32.const 1
      i32.sub
      i32.const 4
      i32.mul
      i32.const 4
      i32.add
      i32.add
      local.get $__temp_check
      i32.store
      local.get $a
      local.set $arr_base_v
      local.get $arr_base_v
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
          i32.const 4
          i32.add
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

почему всё еще выводит букву Т, должно выводить число\
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



## ЗАМЕЧАНИЕ 7

тут всё ещё какие-то проблемы с выводом\
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
I
@
9
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
          local.tee $__temp_check
          local.get $xs
          local.get $i
          i32.const 1
          i32.sub
          i32.const 4
          i32.mul
          i32.const 4
          i32.add
          i32.add
          local.get $__temp_check
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
          i32.const 4
          i32.add
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

