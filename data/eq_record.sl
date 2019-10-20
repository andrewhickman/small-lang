let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: eq true (eq { a: true } { a: true }),
    r: eq false (eq { a: true } { b: false }),
  }