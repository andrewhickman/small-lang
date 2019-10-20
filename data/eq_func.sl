let and = func args => if args.l
  then args.r
  else false
in let or = func args => if args.l
  then true
  else args.r
in
  and {
    l: eq { l: true, r: eq { l: and, r: and } },
    r: eq { l: false, r: eq { l: or, r: and } },
  }