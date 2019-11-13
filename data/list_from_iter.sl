let cmp = import "cmp" in
let list = import "list" in
let iter = import "iter" in

cmp.eq (list.from_iter (iter.range 9 12)) [some: {
  value: 9,
  tail: [some: {
    value: 10,
    tail: [some: {
      value: 11,
      tail: [none],
    }]
  }]
}]
