let cmp = import "cmp" in
let math = import "math" in

let rec fibonacci = func n =>
  if cmp.eq n 0 then 0
  else if cmp.eq n 1 then 1
  else math.add (fibonacci (math.sub n 1)) (fibonacci (math.sub n 2))
in fibonacci 14