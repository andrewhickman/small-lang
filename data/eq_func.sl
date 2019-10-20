let and = func args => if args.l
  then args.r
  else false
in let or = func args => if args.l
  then true
  else args.r
in
  and {
    l: eq true (eq and and),
    r: eq false (eq and or),
  }