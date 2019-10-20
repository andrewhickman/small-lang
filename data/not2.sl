(
  func arg =>
    let not = func x => if x
      then false
      else true
    in 
      not (not arg)
) true