let math = import "math" in

let add4 = func a b c d =>
  math.add (math.add a b) (math.add c d)
in add4 1 2 3 4