let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: eq true (eq false false),
    r: eq false (eq true false),
  }