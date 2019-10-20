let and = func args => if args.l
  then args.r
  else false
in
  and {
    l: eq { l: true, r: eq { l: false, r: false } },
    r: eq { l: false, r: eq { l: true, r: false } },
  }