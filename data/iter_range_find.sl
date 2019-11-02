let cmp = import "cmp" in
let iter = import "iter" in

iter.find (iter.range 3 6) (func n => cmp.eq n 4)