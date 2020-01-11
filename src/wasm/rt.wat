(module
  (memory 1)
  (global $brk (mut i32) (i32.const 0))
  ;; Simple bump allocator
  (func $memory_alloc (param $len i32) (result i32)
    (global.get $brk) ;; The return value
    (global.set $brk (i32.add (global.get $brk) (local.get $len))) ;; Update allocator position

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
  ;; Copy `len` bytes from `src` to `dst`.
  (func $memory_copy (param $src i32) (param $dst i32) (param $len i32)
    (local $src_end i32)

    (local.set $src_end (i32.add (local.get $src) (local.get $len)))

    (block
      (loop
        (br_if 1 (i32.eq (local.get $src) (local.get $src_end)))

        (i32.store8 (local.get $dst) (i32.load8_u (local.get $src)))

        (local.set $src (i32.add (local.get $src) (i32.const 1)))
        (local.set $dst (i32.add (local.get $dst) (i32.const 1)))
        (br 0)
      )
    )
  )
  (func $pair_new (param $first i32) (param $second i32) (result i64)
    (i64.or
      (i64.shl
        (i64.extend_i32_u (local.get $first))
        (i64.const 32)
      )
      (i64.extend_i32_u (local.get $second))
    )
  )
  (func $pair_first (param $val i64) (result i32)
    (i32.wrap_i64
      (i64.shr_u
        (local.get $val)
        (i64.const 32)
      )
    )
  )
  (func $pair_second (param $val i64) (result i32)
    (i32.wrap_i64
      (local.get $val)
    )
  )
  (func $string_new (param $ptr i32) (param $len i32) (result i64)
    (call $pair_new (local.get $ptr) (local.get $len))
  )
  (func $string_add (param $lhs i64) (param $rhs i64) (result i64)
    (local $ptr i32)
    (local $len i32)

    (local.set $ptr
      (call $memory_alloc
        (local.tee $len
          (i32.add
            (call $pair_second (local.get $lhs))
            (call $pair_second (local.get $rhs))
          )
        )
      )
    )

    (call $memory_copy
      (call $pair_first (local.get $lhs))
      (local.get $ptr)
      (call $pair_second (local.get $lhs))
    )
    (call $memory_copy
      (call $pair_first (local.get $rhs))
      (i32.add
        (local.get $ptr)
        (call $pair_second (local.get $lhs))
      )
      (call $pair_second (local.get $rhs))
    )

    (call $string_new (local.get $ptr) (local.get $len))
  )
  (export "memory" (memory 0))
  (export "memory_alloc" (func $memory_alloc))
  (export "memory_copy" (func $memory_copy))
  (export "pair_new" (func $pair_new))
  (export "pair_first" (func $pair_first))
  (export "pair_second" (func $pair_second))
  (export "string_new" (func $string_new))
  (export "string_add" (func $string_add))
)