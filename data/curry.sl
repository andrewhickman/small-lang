let and = func l => 
  func r => if l
    then r
    else false
in
  eq {
    l: and true false,
    r: false,
  }