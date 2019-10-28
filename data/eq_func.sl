let std = import "std" in

let and = func args => if args.l
  then args.r
  else false
in let or = func args => if args.l
  then true
  else args.r
in
  and {
    l: std.eq true (std.eq and and),
    r: std.eq false (std.eq and or),
  }