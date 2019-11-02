let cmp = import "cmp" in

let and = func args => if args.l
  then args.r
  else false
in let or = func args => if args.l
  then true
  else args.r
in
  and {
    l: cmp.eq true (cmp.eq and and),
    r: cmp.eq false (cmp.eq and or),
  }