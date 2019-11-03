let list = import "list" in

list.length [cons: {
  value: true,
  tail: [cons: {
    value: 5,
    tail: [cons: {
      value: "hello",
      tail: [nil],
    }]
  }]
}]