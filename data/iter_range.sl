let cmp = import "cmp" in
let iter = import "iter" in

cmp.eq {
    a: (iter.range 3 6).value,
    b: ((iter.range 3 6).next null).value,
    c: (((iter.range 3 6).next null).next null).value,
    d: ((((iter.range 3 6).next null).next null).next null).value,
} {
    a: [some: 3],
    b: [some: 4],
    c: [some: 5],
    d: [none],
}
