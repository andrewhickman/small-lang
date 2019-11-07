let join = if true
  then func _ => [tag: { a: true }]
  else func _ => [tag: { a: null }]
in
  match join {} with [
    tag: val => if val.a then null else null
  ]