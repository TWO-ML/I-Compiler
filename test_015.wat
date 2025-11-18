(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  ;; Helper function to print i32
  (func $print_i32 (param $value i32)
    (local $buf i32)
    (local.set $buf (i32.const 0))
    ;; Simple implementation: store value in memory and print
    ;; TODO: Implement proper number-to-string conversion
    i32.const 1  ;; stdout
    i32.const 0  ;; iovs pointer
    i32.const 1  ;; iovs_len
    i32.const 0  ;; nwritten pointer
    call $fd_write
    drop
  )

    (func $square
      (param $n i32)
      (result i32)
      local.get $n
      local.get $n
      i32.mul
    )
    (func $Main
      ;; call square
      i32.const 6
      call $square
      call $print_i32
      return
    )
  (func (export "_start")
    call $Main
  )
)
