let iter = import "iter" in
let list = import "list" in

let identity = func n => list.length (list.from_iter (iter.take (iter.range 0 9999) n)) in

identity 6