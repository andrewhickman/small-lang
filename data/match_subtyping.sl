let x = if true
  then [a: true]
  else [b: "hello"]
in match x with [
  a => a,
  b => false,
  c => c,
]