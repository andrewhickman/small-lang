let add = func l r => (__builtin_get_add l) r in

let length = add 1 2 in

let range = func n => add n 0 in
let take = func n => add n 0 in

take (range 0)