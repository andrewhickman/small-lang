let eq = func l r => __builtin_eq { l, r } in
let add = func l r => (__builtin_get_add l) r in
let sub = func l r => (__builtin_get_sub l) r in

let rec fibonacci = func n =>
  if eq n 0 then 0
  else if eq n 1 then 1
  else add (fibonacci (sub n 1)) (fibonacci (sub n 2))
in fibonacci 14