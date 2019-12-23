let rec length = func list => match list with [
  none => 0,
  some: cons => __builtin_get_add 1 (length cons.tail),
] in

length