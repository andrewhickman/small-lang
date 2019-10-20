let x = false
in let f = (
  let x = true
  in func y => x
) in f false
