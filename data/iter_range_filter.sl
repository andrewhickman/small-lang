let cmp = import "cmp" in
let iter = import "iter" in
let bool = import "modules/bool.sl" in

let filtered = iter.filter (iter.range 3 6) (func n => bool.not (cmp.eq n 4)) in

match filtered.next () with [
  some: result => if cmp.eq result.value 3
    then match result.tail.next () with [
      some: result => if cmp.eq result.value 5
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
