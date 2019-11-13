let math = import "math" in

let rec from_iter = func iter => match iter.next () with [
  none => [none],
  some: result => [some: {
    value: result.value,
    tail: from_iter result.tail
  }],
] in

let rec length = func list => match list with [
  none => 0,
  some: cons => math.add 1 (length cons.tail),
] in

{
  from_iter,
  length,
}