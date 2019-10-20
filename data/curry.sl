let and = func l => 
  func r => if l
    then r
    else false
in
  eq (and true false) false