let std = import "std" in

let and = func l => 
  func r => if l
    then r
    else false
in
  std.eq (and true false) false