let std = import "std" in

let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: std.eq true (std.eq false false),
    r: std.eq false (std.eq true false),
  }