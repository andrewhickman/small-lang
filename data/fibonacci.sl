let std = import "std" in

let rec fibonacci = func n =>
  if std.eq n 0 then 0
  else if std.eq n 1 then 1
  else std.add (fibonacci (std.sub n 1)) (fibonacci (std.sub n 2))
in fibonacci 14