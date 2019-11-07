let math = import "math" in

let f = func arg => if true
  then math.add arg.b 1
  else math.add arg.c 1 in

let arg = if true
  then { a: 1, b: 2 }
  else { b: 2, c: 3 } in

f arg