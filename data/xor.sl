(
  let not = func x => if x 
    then false
    else true
  in
    func args => if args.l 
      then not args.r
      else args.r
) { l: true, r: false, j: true }