let cmp = import "cmp" in
let iter = import "iter" in

let mapped = iter.map (iter.range 3 6) (func n => cmp.eq n 4) in

cmp.eq {
    a: mapped.value,
    b: (mapped.next null).value,
    c: ((mapped.next null).next null).value,
    d: (((mapped.next null).next null).next null).value,
} {
    a: [some: false],
    b: [some: true],
    c: [some: false],
    d: [none],
}
