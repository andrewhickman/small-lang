func arg => (
  let xor = (
    func args => (
      if ((args).l) then (
        (args).r
      ) else (
        (args).l
      )
    )
  ) in (
    (xor) ({
      l: arg,
      r: if (arg) then (false) else (true),
    })
  )
)