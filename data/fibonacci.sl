let rec fibonacci = func n =>
  if eq n 0 then 0
  else if eq n 1 then 1
  else add (fibonacci (sub n 1)) (fibonacci (sub n 2))
in fibonacci 23