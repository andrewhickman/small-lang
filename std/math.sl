let cmp = import "cmp" in

{
  add: func l r => __builtin_add { l, r },
  sub: func l r => __builtin_sub { l, r },
}