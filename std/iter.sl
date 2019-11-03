let cmp = import "cmp" in
let math = import "math" in

let rec range = func start => func end => {
  next: func _ => if cmp.eq start end
    then [none]
    else [some: {
      value: start,
      tail: range (math.add start 1) end,
    }],
} in

let rec map = func iter => func f => {
  next: func _ => match iter.next {} with [
    none => [none],
    some: result => [some: {
      value: f result.value,
      tail: map result.tail f,
    }],
  ]
} in

let rec filter = func iter => func f => {
  next: func _ => match iter.next {} with [
    none => [none],
    some: result => if f result.value
      then [some: {
        value: result.value,
        tail: filter result.tail f,
      }]
      else (filter result.tail f).next {},
  ]
} in

let first = func iter => match iter.next {} with [
  some: result => [some: result.value],
  none => [none],
] in

let find = func iter => func f => first (filter iter f) in

{
  range,
  map,
  filter,
  first,
  find,
}