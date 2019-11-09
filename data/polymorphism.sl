let math = import "math" in

let id = func x => x in
{
  int: math.add (id 1) (id 2),
  bool: if id true then null else null,
  record: (id { a: 1 }).a,
  enum: match id [a] with [a => null],
  function: (id math.add) ((id id) 1) (((id id) id) 2)
}