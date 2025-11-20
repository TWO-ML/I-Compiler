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
    ;; Allocate buffer at memory offset 1024
    (local.set $buf (i32.const 1024))
    (local.set $pos (i32.const 1024))
    (local.set $neg (i32.const 0))
    ;; Handle negative numbers
    (if (i32.lt_s (local.get $value) (i32.const 0))
      (then
        (local.set $neg (i32.const 1))
        (local.set $value (i32.sub (i32.const 0) (local.get $value))))
    ))
    ;; Handle zero case
    (if (i32.eq (local.get $value) (i32.const 0))
      (then
        (i32.store8 (local.get $pos) (i32.const 48))
        (local.set $pos (i32.add (local.get $pos) (i32.const 1))))
    ))
    ;; Convert digits (reverse order)
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
    ;; Add minus sign if negative (at start, after reverse)
    (if (i32.eq (local.get $neg) (i32.const 1))
      (then
        ;; Shift string right by 1 byte to make room for minus
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
    ;; Add newline
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
    ;; Reverse the string (digits were stored in reverse)
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
    ;; Write to stdout using fd_write
    (i32.store (i32.const 0) (i32.const 1024))  ;; iovs[0].buf
    (i32.store (i32.const 4) (local.get $len))  ;; iovs[0].len
    (i32.const 1)  ;; stdout
    (i32.const 0)  ;; iovs pointer
    (i32.const 1)  ;; iovs_len
    (i32.const 8)  ;; nwritten pointer
    (call $fd_write)
    drop
  )

  ;; Helper function to print f64
  (func $print_f64 (param $value f64)
    ;; For simplicity, convert to i32 and print (truncate)
    (call $print_i32 (i32.trunc_f64_s (local.get $value)))
  )

    (func $Main
      (local $a i32)
      i32.const 5
      local.set $a
      (local $b i32)
      i32.const 3
      local.set $b
      local.get $a
      local.get $b
      i32.add
      call $print_i32
      local.get $a
      local.get $b
      i32.sub
      call $print_i32
      local.get $a
      local.get $b
      i32.gt_s
      local.get $a
      local.get $b
      i32.ne
      i32.and
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)
