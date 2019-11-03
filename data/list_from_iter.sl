let cmp = import "cmp" in
let list = import "list" in
let iter = import "iter" in

cmp.eq (list.from_iter (iter.range 9 12)) [cons: {
  value: 9,
  tail: [cons: {
    value: 10,
    tail: [cons: {
      value: 11,
      tail: [nil],
    }]
  }]
}]
