let cmp = import "cmp" in

let ff = func _ => (func _ => 4) in
cmp.eq (ff {}) (ff ())