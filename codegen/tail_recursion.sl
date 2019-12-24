let rec any = func list => match list with [
  none => false,
  some: cons => if cons.value
    then true
    else any cons.tail
] in

any