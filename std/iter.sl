let cmp = import "cmp" in
let math = import "math" in

let rec range = func start end => {
  next: func _ => if cmp.eq start end
    then [none]
    else [some: {
      value: start,
      tail: range (math.add start 1) end,
    }],
} in

let rec map = func iter f => {
  next: func _ => match iter.next {} with [
    none => [none],
    some: result => [some: {
      value: f result.value,
      tail: map result.tail f,
    }],
  ]
} in

let rec filter = func iter f => {
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

let rec take = func iter n => {
  next: func _ => match iter.next {} with [
    none => [none],
    some: result => if cmp.eq n 0
      then [none]
      else [some: {
        value: result.value,
        tail: take result.tail (math.sub n 1)
      }]
  ]
} in

let rec length = func iter => match iter.next {} with [
  some: result => math.add 1 (length result.tail),
  none => 0,
] in

let first = func iter => match iter.next {} with [
  some: result => [some: result.value],
  none => [none],
] in

let find = func iter f => first (filter iter f) in

{
  range,
  map,
  filter,
  length,
  first,
  find,
  take,
}