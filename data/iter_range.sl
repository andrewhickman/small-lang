let cmp = import "cmp" in
let iter = import "iter" in

match (iter.range 3 6).next () with [
  some: result => if cmp.eq result.value 3
    then match result.tail.next () with [
      some: result => if cmp.eq result.value 4
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
    else false,
  none => false,
]
