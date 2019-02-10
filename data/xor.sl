func arg => (
  let not = (
    func x => (
      if (x) then (
        false
      ) else (
        true
      )
    )
  ) in (
    let xor = (
      func args => (
        if ((args).l) then (
          (not) ((args).r)
        ) else (
          (args).r
        )
      )
    ) in (
      (xor) ({
        l: arg,
        r: (not) (arg),
      })
    )
  )
)