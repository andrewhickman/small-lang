let cmp = import "cmp" in
let list = import "list" in
let iter = import "iter" in

cmp.eq (list.from_iter (iter.take (iter.range 9 99) 3)) [cons: {
  value: 9,
  tail: [cons: {
    value: 10,
    tail: [cons: {
      value: 11,
      tail: [nil],
    }]
  }]
}]