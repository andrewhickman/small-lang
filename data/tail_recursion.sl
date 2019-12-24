let cmp = import "cmp" in
let math = import "math" in

let rec count = func n =>
  if cmp.eq n 0
    then null
    else count (math.sub n 1) in

count 1000