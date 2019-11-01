let not = func x => if x
  then false
  else true in
let and = func l => func r => if l
  then r
  else false in
let or = func l => func r => if l
  then true
  else r in
{
  not,
  and,
  or,
  xor: func l => func r => or (and l (not r)) (and (not l) r),
}