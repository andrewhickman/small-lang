let and = func args => if args.l
  then args.r
  else false
in
  __builtin_eq {
    l: and,
    r: and,
  }