let record = if true
  then { a: null }
  else { b: null }
in {
  b: record.b,
  c: record.c,
}