let cmp = import "cmp" in

let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: cmp.eq true (cmp.eq { a: true } { a: true }),
    r: cmp.eq false (cmp.eq { a: true } { b: false }),
  }