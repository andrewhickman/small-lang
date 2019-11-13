let list = import "list" in

list.length [some: {
  value: true,
  tail: [some: {
    value: 5,
    tail: [some: {
      value: "hello",
      tail: [none],
    }]
  }]
}]