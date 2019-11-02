let cmp = import "cmp" in
let math = import "math" in

let rec range = func start => func end => {
  value: if cmp.eq start end
    then [none]
    else [some: start],
  next: func _ => range (math.add start 1) end
} in

let rec map = func iter => func f => {
  value: match iter.value with [
    some: value => [some: f value],
    none => [none],
  ],
  next: func _ => map (iter.next null) f,
} in

{
  range,
  map,
}