let join = if false
  then func _ => [tag: { a: true }]
  else func _ => [tag: { a: null }]
in
  match join {} with [
    tag: val => if val.a then null else null
  ]