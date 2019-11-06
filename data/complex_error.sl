let math = import "math" in

let f = if false
  then func record => math.add record.l record.r
  else func record => if true then true else record.r
in
  math.add (f { l: 1, r: 2 }) 2