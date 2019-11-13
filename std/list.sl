let math = import "math" in

let rec from_iter = func iter => match iter.next () with [
  none => [nil],
  some: result => [cons: {
    value: result.value,
    tail: from_iter result.tail
  }],
] in

let rec length = func list => match list with [
  nil => 0,
  cons: cons => math.add 1 (length cons.tail),
] in

{
  from_iter,
  length,
}