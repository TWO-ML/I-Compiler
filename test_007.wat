(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local $len i32)
    (local $neg i32)
    (local $div i32)
    (local $rem i32)
    (local $pos i32)
    (local.set $buf (i32.const 1024))
    (local.set $pos (i32.const 1024))
    (local.set $neg (i32.const 0))
    (if (i32.lt_s (local.get $value) (i32.const 0))
      (then
        (local.set $neg (i32.const 1))
        (local.set $value (i32.sub (i32.const 0) (local.get $value))))
    ))
    (if (i32.eq (local.get $value) (i32.const 0))
      (then
        (i32.store8 (local.get $pos) (i32.const 48))
        (local.set $pos (i32.add (local.get $pos) (i32.const 1))))
    ))
    (block $loop_end
      (loop $loop
        (if (i32.eq (local.get $value) (i32.const 0))
          (then (br $loop_end))
        ))
        (local.set $div (i32.div_u (local.get $value) (i32.const 10)))
        (local.set $rem (i32.rem_u (local.get $value) (i32.const 10)))
        (i32.store8 (local.get $pos) (i32.add (i32.const 48) (local.get $rem)))
        (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
        (local.set $value (local.get $div))
        (br $loop)
      )
    ))
    (if (i32.eq (local.get $neg) (i32.const 1))
      (then
        (local.set $pos (i32.const 1024))
        (block $find_end)
          (loop $find)
            (if (i32.eq (i32.load8_u (local.get $pos)) (i32.const 0))
              (then (br $find_end))
            ))
            (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
            (br $find)
          )
        ))
        (block $shift_end)
          (loop $shift)
            (if (i32.eq (local.get $pos) (i32.const 1024))
              (then (br $shift_end))
            ))
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (local.set $div (i32.load8_u (local.get $pos)))
            (local.set $rem (i32.add (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $rem) (local.get $div))
            (br $shift)
          )
        ))
        (i32.store8 (i32.const 1024) (i32.const 45))
      )
    ))
    (local.set $pos (i32.const 1024))
    (block $len_loop)
      (loop $len)
        (if (i32.eq (i32.load8_u (local.get $pos)) (i32.const 0))
          (then (br $len_loop))
        ))
        (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
        (br $len)
      )
    ))
    (i32.store8 (local.get $pos) (i32.const 10))
    (local.set $len (i32.add (i32.const 1) (i32.sub (local.get $pos) (i32.const 1024))))
    (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
    (block $rev_end
      (loop $rev_loop
        (if (i32.le_u (local.get $buf) (local.get $pos))
          (then (br $rev_end))
        ))
        (local.set $div (i32.load8_u (local.get $buf)))
        (local.set $rem (i32.load8_u (local.get $pos)))
        (i32.store8 (local.get $buf) (local.get $rem))
        (i32.store8 (local.get $pos) (local.get $div))
        (local.set $buf (i32.add (local.get $buf) (i32.const 1)))
        (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
        (br $rev_loop)
      )
    ))
    (i32.store (i32.const 0) (i32.const 1024))
    (i32.store (i32.const 4) (local.get $len))
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 8)
    (call $fd_write)
    drop
  )

  ;; Helper function to print f64
  (func $print_f64 (param $value f64)
    ;; For simplicity, convert to i32 and print (truncate)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $r f64)
      (local $i i32)
      (local $__temp_check i32)
      f64.const 3.490000
      local.set $r
      local.get $r
      f64.nearest
      i32.trunc_f64_s
      local.set $i
      local.get $i
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)
