# Small-lang

A minimal compiler using the mlsub type system
* [Online Demo](https://andrewhickman.dev/)
* [Mlsub paper](https://www.cl.cam.ac.uk/~sd601/thesis.pdf)
* [Reference implementation in OCaml](https://github.com/stedolan/mlsub)
* [Mlsub online demo](https://www.cl.cam.ac.uk/~sd601/mlsub/)

## Features

### Record types
```
let record = { field: 1 }
in record.field
```

### Enum types
```
let enum = [some: 1]
in match enum with [
  some: val => val,
  none => 0,
]
```

### Recursive functions
```
let cmp = import "cmp" in
let math = import "math" in

let rec fibonacci = func n =>
  if cmp.eq n 0 then 0
  else if cmp.eq n 1 then 1
  else math.add
    (fibonacci (math.sub n 1))
    (fibonacci (math.sub n 2))
in fibonacci 14
```