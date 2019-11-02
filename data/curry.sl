let cmp = import "cmp" in

let and = func l =>
  func r => if l
    then r
    else false
in
  cmp.eq (and true false) false