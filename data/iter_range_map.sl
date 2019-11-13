let cmp = import "cmp" in
let iter = import "iter" in

let mapped = iter.map (iter.range 3 6) (func n => cmp.eq n 4) in

match mapped.next () with [
  some: result => if cmp.eq result.value false
    then match result.tail.next () with [
      some: result => if cmp.eq result.value true
        then match result.tail.next () with [
          some: result => if cmp.eq result.value false
            then match result.tail.next () with [
              some: result => false,
              none => true,
            ]
            else false,
          none => false,
        ]
        else false,
      none => false,
    ]
    else false,
  none => false,
]
