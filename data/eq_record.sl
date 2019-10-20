let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: eq { l: true, r: eq { l: { a: true }, r: { a: true } } },
    r: eq { l: false, r: eq { l: { b: true }, r: { a: true } } },
  }