let rec from_iter = func iter => match iter.next {} with [
  none => [nil],
  some: result => [cons: {
    value: result.value,
    tail: from_iter result.tail
  }],
] in

{
  from_iter
}