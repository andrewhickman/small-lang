let cmp = import "cmp" in

{
  add: func l => func r => __builtin_add { l, r },
  sub: func l => func r => __builtin_sub { l, r },
}