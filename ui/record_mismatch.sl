let record = if true
  then { a: null }
  else { b: null }
in {
  c: record.c,
}