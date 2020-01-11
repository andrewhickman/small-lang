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
  (export "memory" (memory 0))
  (export "memory_alloc" (func $memory_alloc))
  (export "memory_copy" (func $memory_copy))
)