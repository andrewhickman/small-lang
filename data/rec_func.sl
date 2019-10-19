let rec f = func x => if x
  then f false
  else true
in f true
