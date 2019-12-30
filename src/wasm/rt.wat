(module
  (memory 1)
  ;; Allocator
  ;;
  ;; The position of the bump-allocator.
  (global $brk (mut i32) (i32.const 0))
  (func $alloc (param $size i32) (result i32)
    (global.get $brk) ;; The return value
    (global.set $brk (i32.add (global.get $brk) (local.get $size))) ;; Update allocator position

    (if
      (i32.eq
        (memory.grow
          (i32.sub
            (i32.div_u
              (global.get $brk)
              (i32.const 65536)
            )
            (i32.sub (memory.size) (i32.const 1))
          )
        )
        (i32.const -1)
      )
      (then
        (unreachable)
      )
    )
  )
  (func $pair_first (param $record i64) (result i32)
    (i32.wrap_i64
      (i64.shr_u
        (local.get $record)
        (i64.const 32)
      )
    )
  )
  (func $pair_second (param $record i64) (result i32)
    (i32.wrap_i64
      (local.get $record)
    )
  )
  (export "memory" (memory 0))
  (export "alloc" (func $alloc))
)